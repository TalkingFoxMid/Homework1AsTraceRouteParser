import scala.sys.process._
import scala.util.{Failure, Success, Try}
import  io.circe._
import  io.circe.generic.auto._
import  io.circe.parser._

object Main {
  case class IpResponse
  (
    var country: Option[String] = None,
    var isp: Option[String] = None,
    var as: Option[String] = None,
    var asname: Option[String] = None,
    var query: String
  )

  final val ipRex = "\\([\\d.]*\\)".r

  def get(url: String): Option[String] = {
    val result = Try {scala.io.Source.fromURL(url)}
    result match {
      case Success(value) => Some(value.mkString)
      case Failure(_) => None
    }
  }

  def getIpInformation(ip: String): Option[IpResponse] = {
    for {
      ipResString <- get(s"http://ip-api.com/json/${ip}?fields=4255743")
      ipResRes <- decode[IpResponse](ipResString).toOption
    } yield ipResRes
  }

  def getFieldRepr(field: Option[String]) = field match {
    case Some(value) => value
    case None => ""
  }

  def main(args: Array[String]) = {
    lazy val traceRouteResult = Process("traceroute 195.201.139.41").lazyLines
    val g = traceRouteResult.drop(2)
      .map(ipRex.findFirstIn(_))
      .foldLeft(List[String]()) {
        case (list: List[String], Some(value)) => list.appended(value.dropRight(1).drop(1))
        case (list, None) => list
      }
      .map(getIpInformation).zipWithIndex
      .map {
        case (Some(IpResponse(country, isp, as, asname, query)), value) =>
          List(Some(value.toString), Some(query), country, isp, as, asname)
            .map(getFieldRepr)
            .foldLeft("")((x, y) => x ++ "| " ++ y)
        case _ => None
      }
      .prepended("query | country | isp | as | asname")
      .foreach(println(_))
  }

}

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
  def getOutput(tracerouteLines: List[String]): Unit = {
    tracerouteLines.drop(2) // из вывода traceroute выбрасываем 2 первые строки
      .map(ipRex.findFirstIn(_)) // регуляркой в каждой находим ip
      .foldLeft(List[String]()) { // фильтруем всё кроме айпишников (убираем скобочки)
        case (list: List[String], Some(value)) => list.appended(value.dropRight(1).drop(1))
        case (list, None) => list
      }
      .map(getIpInformation).zipWithIndex // узнаём информацию о каждом из айпишников, получаем JSON-чик
      .map { // далее объекты IpResponse парсим в строковое представление.
        case (Some(IpResponse(country, isp, as, asname, query)), value) =>
          List(Some(value.toString), Some(query), country, isp, as, asname)
            .map(getFieldRepr)
            .foldLeft("")((x, y) => x ++ "| " ++ y)
        case _ => None
      }
      .prepended("id | query | country | isp | as | asname")
      .foreach(println(_)) // распечатываем
  }

  def main(args: Array[String]) = {
    args match { // матчимся по аргументам
      case Array(ip) => Try { // если массив из одного элемента,
        Process("traceroute " + ip).lazyLines.toList // то поднимаем процесс traceroute с этим аргументом
      } match {
        case Success(lines) => getOutput(lines) // если процесс поднялся, запускаем функцию getOutput
        case Failure(exception) => print(exception)
      }
      case _ => print("Not enough args.")
    }

  }
}

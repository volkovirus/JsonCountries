case class CountryNames(official: String)
case class Country (name: CountryNames, capital: List[String], region: String, area: Int)
case class CountryNew (name: String, capital: String, area: Int)

object example extends App {
  import scala.io.Source
  import java.io.FileOutputStream
  import java.io.PrintStream
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  def src = Source.fromURL("https://raw.githubusercontent.com/mledoze/countries/master/countries.json","UTF8")

  def decodeListTolerantly[A: Decoder]: Decoder[List[A]] =
    Decoder.decodeList(Decoder[A].either(Decoder[Json])).map(
      _.flatMap(_.left.toOption)
    )
  val myTolerantCountryDecoder = decodeListTolerantly[Country]

  val listCountry = decode(src.getLines().mkString)(myTolerantCountryDecoder).getOrElse(List[Country]())
  val filteredListCountry = listCountry.filter(_.region == "Africa").sortBy(- _.area).take(10)
  val resultJson = (for (country <- filteredListCountry) yield CountryNew(country.name.official,country.capital match {case header::body => header case _ => ""},country.area)).asJson

  val fileName: String = if (args.nonEmpty) args(0) else "default.json"

  val fos = new FileOutputStream(fileName)
  val ps = new PrintStream(fos)

  ps.print(resultJson)
}

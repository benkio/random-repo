package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

object AirportQueries {
  import Database.airportsTable
  import Database.countryTable
  import Database.runawaysTable

  def airportsByCountryCode(countryCode : String, offset : Int, pageLength: Int ) : Query[Airport] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) select (airport)
  }.page(offset, pageLength)

  def getNumberOfAirportsByCountry(countryCode : String) : Query[Measures[Long]] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) compute(count)
  }

  def getAirportDenseCountries(numberOfResult : Int, isDesc : Boolean) = join(countryTable, airportsTable.leftOuter) {
    (country, airport) => groupBy(country.name) compute(count()) orderBy(Database.countOrdering(isDesc)) on(Some(country.code) === airport.map(_.iso_country))
  }.page(0, numberOfResult)

  def airportInRunaways = from(airportsTable) {
    a => where (a.id in (from(runawaysTable){ r => select(r.airport_ref)} )) select (a)
  }

  def airportByCountry(countriesCode : List[String]) : Query[(String, Int)] = from(airportInRunaways, countryTable) {
    (a, c) => where (a.iso_country === c.code and (c.code in (countriesCode))) select(c.name, a.id)
  }
}

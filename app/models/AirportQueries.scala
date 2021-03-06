package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

object AirportQueries {
  import DatabaseSchema.airportsTable
  import DatabaseSchema.countryTable
  import DatabaseSchema.runawaysTable

  // Based on the boolean return the orderby count arg with ascending or descending
  def countOrdering[T](column : TypedExpressionNode[T], isDesc : Boolean) = {
    if (isDesc) count(column) desc else count(column) asc
  }

  def airportsByCountryCode(countryCode : String, offset : Int, pageLength: Int ) : Query[Airport] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) select (airport)
  }.page(offset, pageLength)

  def getNumberOfAirportsByCountry(countryCode : String) : Query[Measures[Long]] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) compute(count)
  }

  def getAirportDenseCountries(numberOfResult : Int, isDesc : Boolean) = join(countryTable, airportsTable.leftOuter) {
    (country, airport) => groupBy(country.name) compute(count(airport.map(_.id))) orderBy(countOrdering(airport.map(_.id), isDesc)) on(Some(country.code) === airport.map(_.iso_country))
  }.page(0, numberOfResult)

  def airportInRunaways = from(airportsTable) {
    a => where (a.id in (from(runawaysTable){ r => select(r.airport_ref)} )) select (a)
  }

  def airportByCountry(countriesCode : List[String]) : Query[(String, Int)] = from(airportInRunaways, countryTable) {
    (a, c) => where (a.iso_country === c.code and (c.code in (countriesCode))) select(c.name, a.id)
  }
}

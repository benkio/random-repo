package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

object AirportQueries {
  import Database.airportsTable

  def countOrdering(isDesc : Boolean) : ExpressionNode = {
    if (isDesc) count() desc else count() asc
  }

  def airportsByCountryCode(countryCode : String, offset : Int, pageLength: Int ) : Query[Airport] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) select (airport)
  }.page(offset, pageLength)

  def getNumberOfAirportsByCountry(countryCode : String) : Query[Measures[Long]] = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) compute(count)
  }

  def getAirportDenseCountries(numberOfResult : Int, isDesc : Boolean) : Query[GroupWithMeasures[String,Long]] = from(airportsTable) {
    airport => groupBy(airport.iso_country) compute(count()) orderBy(countOrdering(isDesc))
  }.page(0, numberOfResult)
}

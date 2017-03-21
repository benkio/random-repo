package models

import org.squeryl.Schema
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import play.api.cache._
import play.api.mvc._
import javax.inject.Inject

import org.squeryl.dsl._
import org.squeryl.dsl.ast._

/**** Database case Classes *******/

case class Country(id             : Int,
                   code           : String,
                   name           : String,
                   continent      : String,
                   wikipedia_link : String,
                   keywords       : Seq[String])
case class Airport(id                : Int,
                    ident             : String,
                    airport_type      : String,
                    name              : String,
                    latitude_deg      : Float,
                    longitude_deg     : Float,
                    elevation_ft      : Option[Int],
                    iso_country       : String)
case class Runaway(id                        : Int,
                   airport_ref               : Int,
                   airport_ident             : String,
                   length_ft                 : Option[Int],
                   width_ft                  : Option[Int],
                   surface                   : Option[String],
                   lighted                   : Boolean,
                   closed                    : Boolean)

// Cointains all the queries from the controllers
object DatabaseSchema extends Schema {
  // Squeryl initialization

  val airportsTable: Table[Airport] =
    table[Airport]("airports")
  val runawaysTable: Table[Runaway] =
    table[Runaway]("runaways")
  val countryTable: Table[Country] =
    table[Country]("countries")

  on(airportsTable) { a => declare {
    a.id is(indexed, unique)
    a.airport_type is(named("type"))
  }}
  on(runawaysTable) { r => declare {
    r.id is(indexed, unique)
  }}
  on(countryTable) { c => declare {
    c.id   is(indexed, unique)
    c.code is(unique)
  }}

}

class Database(cache: CacheApi) {

  // Perform a transaction with different queries to get the search result, paginated
  def airportRunawayQuery(countryNameOrCode : String, offset: Int, pageLength: Int) : Try[(List[Airport], List[Runaway], Long)] = Try(inTransaction {
      val countryCode = CountryQueries.countryByCodeOrName(countryNameOrCode).single
      val airports = AirportQueries.airportsByCountryCode(countryCode, offset, pageLength).toList
      val airportsCount : Long = AirportQueries.getNumberOfAirportsByCountry(countryCode).single.measures
      val runaways = RunawayQueries.runawaysInAirport(airports).toList
      (airports, runaways, airportsCount)
   })

  // Return a list of countries code and names
  def countries: Try[List[Country]] = Try(inTransaction { CountryQueries.countries.toList } )

  // Return a list of countries paginated
  def countries(pageLength : Int, offset : Int): Try[List[Country]] = Try(inTransaction { CountryQueries.countries(pageLength, offset).toList})

  // Return the number of airports by country with the specified order and number of results
  def getAirportDenseCountries(numberOfResult : Int, isDesc : Boolean) : Try[List[(String, Long)]] = cache.getOrElse[Try[List[(String, Long)]]]("airportDenseCountries" + numberOfResult + "orderingDesc" + isDesc) {
    Try(inTransaction { AirportQueries.getAirportDenseCountries(numberOfResult, isDesc).toList map {t => (t.key, t.measures)}})
  }

  def runawaySurfaceAndAirportRef = Try(inTransaction {
                                      RunawayQueries.runawaySurfaceAndAirportRef.toList
                                        })

  def airportByCountry(limitContries: Int, offsetCountries: Int) = Try(inTransaction {
                                                                         val filteredCountryCodes = CountryQueries.countries(limitContries, offsetCountries).toList.map(_.code)
                                                                         AirportQueries.airportByCountry(filteredCountryCodes).toList
                                                                       })
}

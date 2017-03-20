package models

import org.squeryl.Schema
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import org.squeryl.dsl._
import org.squeryl.dsl.ast._


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
//                    continent         : String,
                    iso_country       : String)
//                    iso_region        : String,
//                    municipality      : Option[String],
//                    scheduled_service : Boolean,
//                    gps_code          : Option[String],
//                    iata_code         : Option[String],
//                    local_code        : Option[String],
//                    home_link         : Option[String],
//                    wikipedia_link    : Option[String],
//                    keywords          : Seq[String])
case class Runaway(id                        : Int,
                   airport_ref               : Int,
                   airport_ident             : String,
                   length_ft                 : Option[Int],
                   width_ft                  : Option[Int],
                   surface                   : Option[String],
                   lighted                   : Boolean,
                   closed                    : Boolean)
/*                   le_ident                  : Option[String],
                   le_latitude_deg           : Option[Float],
                   le_longitude_deg          : Option[Float],
                   le_elevation_ft           : Option[Int],
                   le_heading_degT           : Option[Float],
                   le_displaced_threshold_ft : Option[Int],
                   he_ident                  : Option[String],
                   he_latitude_deg           : Option[Float],
                   he_longitude_deg          : Option[Float],
                   he_elevation_ft           : Option[Int],
                   he_heading_degT           : Option[Float],
                   he_displaced_threshold_ft : Option[Int])*/

object Database extends Schema {

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

  // Based on the boolean return the orderby count arg with ascending or descending
  def countOrdering(isDesc : Boolean) = {
    if (isDesc) count() desc else count() asc
  }

  // Perform a transaction with different queries to get the search result
  def airportRunawayQuery(countryNameOrCode : String, offset: Int, pageLength: Int) : Try[(List[Airport], List[Runaway], Long)] = Try(inTransaction {
      val countryCode = CountryQueries.countryByCodeOrName(countryNameOrCode).single
      val airports = AirportQueries.airportsByCountryCode(countryCode, offset, pageLength).toList
      val airportsCount : Long = AirportQueries.getNumberOfAirportsByCountry(countryCode).single.measures
      val runaways = RunawayQueries.runawaysInAirport(airports).toList
//    val airportsWithRunaways = JoinQueries.runawaysInAirport(AirportQueries.airportsByCountryCode(countryCode, offset, pageLength)).toList

      (airports, runaways, airportsCount)
   })

  // Return a list of countryes code and names
  def countries: Try[List[Country]] = Try(inTransaction { CountryQueries.countries.toList})
  def countries(pageLength : Int, offset : Int): Try[List[Country]] = Try(inTransaction { CountryQueries.countries(pageLength, offset).toList})

  // Return the number of airports by country with the specified order and number of results
  def getAirportDenseCountries(numberOfResult : Int, isDesc : Boolean) : Try[List[(String, Long)]] = Try(inTransaction { AirportQueries.getAirportDenseCountries(numberOfResult, isDesc).toList map {t => (t.key, t.measures)}
                                                                                                         })

  def runawaySurfaceAndAirportRef = Try(inTransaction {
                                      RunawayQueries.runawaySurfaceAndAirportRef.toList
                                        })

  def airportByCountry(limitContries: Int, offsetCountries: Int) = Try(inTransaction {
                                                                         AirportQueries.airportByCountry(CountryQueries.countries(limitContries, offsetCountries)).toList
                                                                       })
}

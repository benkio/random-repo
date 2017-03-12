package models

import org.squeryl.Schema
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table


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
  }}
  on(runawaysTable) { r => declare {
    r.id is(indexed, unique)
  }}
  on(countryTable) { c => declare {
    c.id   is(indexed, unique)
    c.code is(unique)
  }}

  // Perform the following operations
  // 1 - from the country name/code get the country code
  // 2 - fetch the airports/runaways with that country code
  // 3 - organize all in a map 
  def airportRunawayQuery(countryNameOrCode : String, offset: Int, pageLength: Int) : Map[Airport, Seq[Runaway]] = {
    val countryCode : String = inTransaction { CountryDataAccess.countryByCodeOrName(countryNameOrCode).single }
    val data = inTransaction { RunawayDataAccess.runawaysInAirport(countryCode, offset, pageLength).toList }
    data groupBy {_._1} map {
      case (a,l) => a -> l.map{case (a,r) => r}
    }
  }
}
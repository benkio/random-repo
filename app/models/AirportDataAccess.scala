package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable

object AirportDataAccess {
  import Database.airportsTable

  def airportsByCountryCode(countryCode : String, offset : Int, pageLength: Int ) = from(airportsTable) {
    airport => where(airport.iso_country === countryCode) select (airport)
  }.page(offset, pageLength)
}

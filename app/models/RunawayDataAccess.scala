package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable

object RunawayDataAccess {
  import Database.runawaysTable
  import Database.airportsTable

  def runawaysInAirport(countryCode : String, offset: Int, pageLength: Int) : Query[(Airport, Runaway)] = join(airportsTable, runawaysTable) {
    (airport, runaway) =>
    where(airport.iso_country === countryCode).
    select(airport, runaway).
    on(runaway.airport_ref === airport.id and runaway.airport_ident === airport.ident)
  }.page(offset, pageLength)
}

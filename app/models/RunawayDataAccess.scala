package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query

object RunawayDataAccess {
  import Database.runawaysTable
  import Database.airportsTable

  def runawaysInAirport(airports : List[Airport]) : Query[Runaway] =
    from(runawaysTable) {
      runaway =>
      where (  (runaway.airport_ref in (airports map(_.id))) and
               (runaway.airport_ident in (airports map (_.ident)))
      )
      select(runaway)
    }
}

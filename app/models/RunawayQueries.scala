package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query

import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import org.squeryl.dsl.fsm._


object RunawayQueries {
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

  def runawaySurfaceAndAirportRef  = from (runawaysTable) {
    runaway => where (runaway.surface isNotNull) groupBy (runaway.surface, runaway.airport_ref)
  }
}

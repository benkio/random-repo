package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import models._
import shared._
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

@Singleton
class HomeController @Inject() extends Controller {

  def index = Action { implicit request =>
    val countrySuggestions = Database.getAllCountryCodeAndNames getOrElse (List()) map(_._2)
    Ok(views.html.index(countrySuggestions))
  }

  def report = Action { implicit request =>
    val countryWithMostAirports = Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, true)
    val countryWithLessAirports = Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, false)


    val result : List[List[(String, Long)]] = List(countryWithMostAirports, countryWithLessAirports) map (_.getOrElse(List()))
    Ok(views.html.report(result(0), result(1)))
  }

  def query(countryNameOrCode : String, pageNumber : Int) = Action { implicit request =>
    val (airports : List[Airport], runaways : List[Runaway], airportsCount : Long) = Database.airportRunawayQuery(countryNameOrCode,
                                                                                                                  PaginationLogic.getOffset(pageNumber),
                                                                                                                  PaginationLogic.paginationLength) getOrElse ((List(), List(),1L))

    val airportsAndRunawaysGrouped = airports zip runaways groupBy {_._1} map { case (a,l) => { a -> l.map { case (a,r) => r } } } map { case (a,r) => a -> r.toSeq }

    Ok(views.html.querySegment(airportsAndRunawaysGrouped,
                               pageNumber,
                               countryNameOrCode,
                               PaginationLogic.getTotalPages(airportsCount)))
  }
}

object PaginationLogic {

  val paginationLength = 50;

  def getOffset(pageNumber : Int) : Int = {
    (pageNumber - 1) * paginationLength
  }

  def getTotalPages(totalRecord : Long) : Int = {
    totalRecord.toInt/paginationLength
  }
}

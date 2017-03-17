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

  private def countryCodeToName(densityDataset : List[(String, Long)], countryCodeNames : List[(String, String)]) = {
      densityDataset map { case d => ( (countryCodeNames.find(t => t._1 == d._1).map(_._2).getOrElse(d._1)), d._2) }
  }

  def index = Action { implicit request =>
    val countrySuggestions = Database.getAllCountryCodeAndNames getOrElse (List()) map(_._2)
    Ok(views.html.index(countrySuggestions))
  }

  def report = Action { implicit request =>
    val countryWithMostAirports = Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, true) getOrElse (List())
    val countryWithLessAirports = Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, false) getOrElse (List())
    val countryCodeNames = Database.getAllCountryCodeAndNames getOrElse (List())
    Ok(views.html.report(countryCodeToName(countryWithMostAirports, countryCodeNames), countryCodeToName(countryWithLessAirports, countryCodeNames)))
  }

  def query(countryNameOrCode : String, pageNumber : Int) = Action { implicit request =>
    val offset = (Shared.paginationLength - 1) * pageNumber

    val (airports : List[Airport], runaways : List[Runaway], airportsCount : Long) = Database.airportRunawayQuery(countryNameOrCode, offset, Shared.paginationLength) getOrElse ((List(), List(),1L))

    val totalPages : Int = (airportsCount.toInt/Shared.paginationLength)
    val airportsAndRunawaysGrouped = airports zip runaways groupBy {_._1} map { case (a,l) => { a -> l.map { case (a,r) => r } } } map { case (a,r) => a -> r.toSeq }

    Ok(views.html.querySegment(airportsAndRunawaysGrouped, pageNumber, countryNameOrCode, totalPages))
  }
}

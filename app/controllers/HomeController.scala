package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import models._
import shared._
import controllers.utils._
import scala.concurrent.duration._
import scala.concurrent._
import scala.util._
import ExecutionContext.Implicits.global

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

/*
 NEED TO RUN ASYNCHRONOUSLY FOR FIRST 10 MOST POPULATED COUNTRIES
 USE PAGINATION AND PUT THEM LAST

 SELECT DISTINCT C.ISO_COUNTRY, B.SURFACE
 FROM (SELECT SURFACE, AIRPORT_REF, AIRPORT_IDENT FROM RUNAWAYS WHERE SURFACE IS NOT NULL) AS B
 JOIN AIRPORTS C
 ON B.AIRPORT_REF = C.ID AND B.AIRPORT_IDENT = C.IDENT
 WHERE C.ISO_COUNTRY IN ('AE', 'AF', 'AO', 'AM', 'AL' )
 */

@Singleton
class HomeController @Inject() extends Controller {

  val allCountriesCodeAndNames : Future[Try[List[(String,String)]]] = Future {
    Database.getAllCountryCodeAndNames
  }

  val reportMostAirportDensity : Future[Try[List[(String, Long)]]] = Future {
    Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, true)
  }
  val reportLessAirportDensity : Future[Try[List[(String, Long)]]] = Future {
    Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, false)
  }
  val reportSurfaceTypeRunawayPerCountriesChunk1 : Future[Try[List[(String, List[String])]]] = Future {
    val countriesWithMostAirports = Await.result(reportMostAirportDensity, 10 seconds)
    val countriesCodeAndNames = Await.result(allCountriesCodeAndNames, 10 seconds)
    countriesWithMostAirports map { l => {
                                     val countriesCodes = countriesCodeAndNames.filter(t => l.map(_._1).contains(t.map(_._2))).map(x => x.map(_._1))
                                     ???
                                   }
    }
  }

  // Fetch from database the list of countries and pass them in the index page
  def index = Action { implicit request =>
    val countrySuggestions = Await.result(allCountriesCodeAndNames, 10 seconds) map(_.map(_._2))
    ErrorHandler.checkForErrors(countrySuggestions, (l : List[String]) => Ok(views.html.index(l)))
  }

  // Fetch from database the list of most and less countries by airports density and pass them to the
  // Report page
  def report = Action { implicit request =>
    val result = List(Await.result(reportMostAirportDensity, 10 seconds),
                      Await.result(reportLessAirportDensity, 10 seconds))
    ErrorHandler.checkForErrors(result, (l : List[List[(String, Long)]]) => Ok(views.html.report(l(0), l(1))))
  }

  // Perfom the search with the input, regroup the data properly and pass it to the query page
  def query(countryNameOrCode : String, pageNumber : Int) = Action { implicit request =>
    val result /*(airports : List[Airport], runaways : List[Runaway], airportsCount : Long)*/ = Database.airportRunawayQuery(countryNameOrCode,
                                                                                                                  PaginationLogic.getOffset(pageNumber),
                                                                                                                  PaginationLogic.paginationLength)

    ErrorHandler.checkForErrors(result, (t : (List[Airport], List[Runaway], Long)) => {

        val airportsAndRunawaysGrouped = t._1 zip t._2 groupBy {_._1} map { case (a,l) => { a -> l.map { case (a,r) => r } } } map { case (a,r) => a -> r.toSeq }

        Ok(views.html.querySegment(airportsAndRunawaysGrouped,
                                   pageNumber,
                                   countryNameOrCode,
                                   PaginationLogic.getTotalPages(t._3)))
    })
  }

  def error(message : String) = Action {
    Ok(views.html.error(message))
  }
}

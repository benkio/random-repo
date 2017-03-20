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
import scala.collection.parallel.immutable._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

@Singleton
class HomeController @Inject() extends Controller {

  val timeout = Duration.Inf

  val countries : Future[Try[List[Country]]] = Future {
    Database.countries
  }

  val reportMostAirportDensity : Future[Try[List[(String, Long)]]] = Future {
    Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, true)
  }
  val reportLessAirportDensity : Future[Try[List[(String, Long)]]] = Future {
    Database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, false)
  }

  /*
  val reportRunawaySurfacePerCountry : List[Future[Try[ParMap[String, List[String]]]]] = {
    val cs = Await.result(countries, timeout)
    val totalPages : Try[Int] = cs.map( l => { PaginationLogic.getTotalPages(l.length.toLong) })
    totalPages map { tp =>
      (1 to tp).map(i => reportRunawaySurfacePerCountry(PaginationLogic.paginationLength, PaginationLogic.getOffset(i))).toList
    } getOrElse (List())
  }
   */
  def reportRunawaySurfacePerCountry(pageLength: Int, offset: Int) : Future[Try[Map[String, List[String]]]] = Future {
    /*for {
      x <- Database.airportByCountry(pageLength, offset)
      y <- Database.runawaySurfaceAndAirportRef

      } yeld
     */
    ???
  }
   
  // Fetch from database the list of countries and pass them in the index page
  def index = Action { implicit request =>
    val countrySuggestions = Await.result(countries, 10 seconds) map(_.map(_.name))
    ErrorHandler.checkForErrors(countrySuggestions, (l : List[String]) => Ok(views.html.index(l)))
  }

  // Fetch from database the list of most and less countries by airports density and pass them to the
  // Report page
  def report(pageNumber : Int) = Action { implicit request =>
    val result1 = List(Await.result(reportMostAirportDensity, timeout),
                       Await.result(reportLessAirportDensity, timeout))
    ErrorHandler.checkForErrors(result1,
                                (l : List[List[(String, Long)]]) => {
                                  val data = Await.result(reportRunawaySurfacePerCountry(PaginationLogic.paginationLength, PaginationLogic.getOffset(pageNumber)), timeout)
                                  ErrorHandler.checkForErrors(data, (r : Map[String, List[String]]) => Ok(views.html.report(l(0), l(1), r, pageNumber, PaginationLogic.getTotalPages(r.size))))
                                })
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

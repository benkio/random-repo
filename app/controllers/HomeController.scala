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
import org.squeryl.dsl._
import org.squeryl.dsl.ast._


import play.api.cache._
import play.api.mvc._
import javax.inject.Inject

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */

@Singleton
class HomeController @Inject() (cache: CacheApi) extends Controller {

  val database = new Database(cache)

  val paginationLogic1 = new PaginationLogic(50)
  val paginationLogic2 = new PaginationLogic(25)

  def reportRunawaySurfacePerCountry(pageLength: Int, offset: Int) : Try[Map[String, List[String]]] =
    cache.getOrElse[Try[Map[String, List[String]]]]("reportRunawaySurfacePerCountry"+ pageLength + offset){
      Grouper.groupCountryCodeAndSurfaces(database.airportByCountry(pageLength, offset), database.runawaySurfaceAndAirportRef)
    }
  // Fetch from database the list of countries and pass them in the index page
  def index = Action { implicit request =>
    val countrySuggestions = database.countries map(_.map(_.name))
    ErrorHandler.checkForErrors(countrySuggestions, (l : List[String]) => Ok(views.html.index(l)))
  }

  // Fetch from database the list of most and less countries by airports density and pass them to the
  // Report page
  def report() = Action { implicit request =>
    val reportResult1 = List(database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, true),
                             database.getAirportDenseCountries(Shared.reportNumberCountryInDensity, false))
    val reportResult2 = reportRunawaySurfacePerCountry(paginationLogic2.paginationLength, paginationLogic2.getOffset(1))
    val reportResult3 = database.countries.map(_.length)
    ErrorHandler.checkForErrors(reportResult1,
                                (l : List[List[(String, Long)]]) => {
                                  ErrorHandler.checkForErrors(reportResult2,
                                                              (r : Map[String, List[String]]) =>
                                    ErrorHandler.checkForErrors(reportResult3,
                                                                (s : Int) =>

                                      Ok(views.html.report(l(0),
                                                           l(1),
                                                           r,
                                                           1,
                                                           paginationLogic2.paginationLength,
                                                           paginationLogic2.getTotalPages(s) ))))
                                })
  }

  def reportPage(pageNumber : Int) = Action { implicit request =>
    val reportResult = reportRunawaySurfacePerCountry(paginationLogic2.paginationLength, paginationLogic2.getOffset(pageNumber))
    val reportResult3 = database.countries.map(_.length)
    ErrorHandler.checkForErrors(reportResult,
                                (r : Map[String, List[String]]) =>
      ErrorHandler.checkForErrors(reportResult3,
                                  (s : Int) =>
        Ok(views.html.reportSegment(r,
                             pageNumber,
                             paginationLogic2.paginationLength,
                             paginationLogic2.getTotalPages(s)))))
  }


  // Perfom the search with the input, regroup the data properly and pass it to the query page
  def query(countryNameOrCode : String, pageNumber : Int) = Action { implicit request =>
    /*(airports : List[Airport], runaways : List[Runaway], airportsCount : Long)*/
    val result = database.airportRunawayQuery(countryNameOrCode,
                                              paginationLogic1.getOffset(pageNumber),
                                              paginationLogic1.paginationLength)

    ErrorHandler.checkForErrors(result, (t : (List[Airport], List[Runaway], Long)) => {

                                  val airportsAndRunawaysGrouped = Grouper.groupAirportsAndRunaways(t._1, t._2)

                                  Ok(views.html.querySegment(airportsAndRunawaysGrouped,
                                                             pageNumber,
                                                             countryNameOrCode,
                                                             paginationLogic1.getTotalPages(t._3),
                                                             paginationLogic1.paginationLength))
                                })
  }

  def error(message : String) = Action {
    Ok(views.html.error(message))
  }
}

object Grouper {
  def groupAirportsAndRunaways( airports : List[Airport], runaways : List[Runaway]) : Map[Airport, Seq[Runaway]] = airports.map(a => a -> runaways.filter(r => r.airport_ref == a.id && r.airport_ident == a.ident) ) toMap

  def groupCountryCodeAndSurfaces(countryAndAirportID : Try[List[(String, Int)]], surfaceByAirportID : Try[List[org.squeryl.dsl.Group[Product2[Option[String],Int]]]]) : Try[Map[String, List[String]]] = {
    countryAndAirportID.flatMap(x =>
      surfaceByAirportID.map(y =>
        x.flatMap{ case (c, aid) => y.map(_.key) filter{
                    case (s, aid2) => aid == aid2 } map {
                    case (s, aid2) => (c,s)
                  }
        }.distinct
      ).map(xs =>
        xs.groupBy(_._1) map {
          case (c, l) => c -> l.filterNot(t => t._2.isEmpty).map(_._2.get)
        } toMap
      )
    )
  }

}

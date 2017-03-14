package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import models.Database

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  val queryPageLength = 50; // Find a way to share it with client flawlessly

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def report = Action { implicit request =>
    Ok(views.html.report())
  }

  def query(countryNameOrCode : String, pageNumber : Int) = Action { implicit request =>
    val airportsAndRunaways = Database.airportRunawayQuery(countryNameOrCode, queryPageLength * pageNumber, queryPageLength)
    Ok(views.html.querySegment(Map(), pageNumber))
  }
}

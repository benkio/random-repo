package controllers

import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.Results._
import scala.util._
import org.scalatest.Inspectors.{forAll}
import org.scalatest.prop.GeneratorDrivenPropertyChecks //ScalaCheck
import controllers.utils._
import play.mvc._
import scala.util._

class ErrorHandlerSpec extends PlaySpec {

  def failureTry = Try(throw new Exception(""))
  def successTry = Try(true)

  val failureList = List(  List(successTry, failureTry, successTry),
                           List(successTry, failureTry),
                           List(failureTry, successTry))

  val successList = List ( List(successTry, successTry, successTry),
                           List(successTry, successTry, successTry),
                           List(successTry, successTry, successTry, successTry, successTry) )

  "Error Handler" should {
    "return Redirect in case of Failure" in {
      forAll(failureList) {l =>
        val result : play.api.mvc.Result = ErrorHandler.checkForErrors(l, (x : List[Boolean]) => Ok(views.html.error("")))
        result mustBe (Redirect(controllers.routes.HomeController.error("")))
      }
      val result2 : play.api.mvc.Result = ErrorHandler.checkForErrors(failureTry, (v: Option[_]) => Ok(views.html.error("")))
      result2 mustBe (Redirect(controllers.routes.HomeController.error("")))
    }

    "return the previded result in case of Success" in {
      forAll(successList) {l =>
        val result : play.api.mvc.Result = ErrorHandler.checkForErrors(l, (x : List[Boolean]) => Ok(views.html.error("")))
        result mustBe (Ok(views.html.error("")))
      }
      val result2 : play.api.mvc.Result = ErrorHandler.checkForErrors(successTry, (v: Boolean) => Ok(views.html.error("")))
      result2 mustBe (Ok(views.html.error("")))
    }
  }
}

package controllers.utils

import play.api.mvc.Results._
import scala.util._

object ErrorHandler {

  def checkForErrors[T](result : List[Try[T]], happyPath : List[T] => play.api.mvc.Result) : play.api.mvc.Result = {
    if (result.filter(_.isFailure).isEmpty) {
      happyPath(result collect { case Success(x) => x })
    }else {
      val errorMessage : String = result collect { case Failure(e) => e.getMessage } reduce {(e1, e2) =>  "\n "+ e1 + "\n " + e2  }
      Redirect(controllers.routes.HomeController.error(errorMessage))
    }
  }

  def checkForErrors[T](result : Try[T], happyPath : T => play.api.mvc.Result) : play.api.mvc.Result = {
    result match {
      case Success(v) => happyPath(v)
      case Failure(e) => Redirect(controllers.routes.HomeController.error(e.getMessage))
    }
  }
}

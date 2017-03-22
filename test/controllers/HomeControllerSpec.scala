package controllers

import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import org.scalatest.Inspectors.{forAll}

import play.api.cache._
import play.api.mvc._
import scala.concurrent._

import test.mock._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with OneAppPerTest {

  val headerKeywords = List( "</nav>", routes.HomeController.report.toString, routes.HomeController.index.toString, "navbar-nav", "navbar-header" )
  val mainKeywords = List( "<title>", "jquery", "bootstrap", "container", "PageContent")
  val indexKeywords = List("<script>", "queryPage")
  val queryKeywords = List("querySuggestions", "queryContent", "querySearch")
  val querySegmentKeywords = List("runwayAirport", "airportAndRunawayByCountry")
  val reportKeywords = List("changePage", "mostAirportDenseCountry", "lessAirportDenseCountry", "reportSegment", "</table>", "</tbody>")
  val reportSegmentKeywords = List("runawaySurfacePerCountry", "</table>", "</tbody>" )
  val paginationSegmentKeywords = List("pagination","Pagination-nav", "previousPage", "nextPage", "page-link", "page-number")
  val errorKeywords : List[String] = List("jumbotron", routes.HomeController.index.toString, "errorMessage")

  def checkKeywords(contentBodies : List[String], keywords : List[String], not : Boolean = false) {
    forAll (contentBodies) {body =>
      forAll (keywords) { d => if (not) body mustNot (include (d)) else body must include (d) }
    }
  }

  def checkRoute(allKeywords : List[String], homes : List[Future[Result]], not : Boolean = false) {
    val contents = homes.map(contentAsString)

    forAll(homes) {h => {
                     if (not)
                       status(h) mustBe SEE_OTHER
                     else {
                       status(h) mustBe OK;
                       contentType(h) mustBe Some("text/html")
                     }
                   }
    }
    checkKeywords(contents, allKeywords, not)
  }

  "HomeController GET" should {

    "render the index page from a new instance of controller, inside application and in localhost" in {
      val controller = new HomeController( new MockCacheApi())
      val home = controller.index().apply(FakeRequest())

      val controllerApp = app.injector.instanceOf[HomeController]
      val homeApp = controllerApp.index().apply(FakeRequest())

      val requestLocalhost = FakeRequest(GET, "/").withHeaders("Host" -> "localhost")
      val homeLocalhost = route(app, requestLocalhost).get

      val homes = List(home, homeApp, homeLocalhost)
      val allKeywords = headerKeywords ++ mainKeywords ++ indexKeywords ++ queryKeywords

      checkRoute(allKeywords, homes, false)
    }

    "render the report page from a new instance of controller, inside application and in localhost" in {
      val controller = new HomeController(new MockCacheApi)
      val home = controller.report().apply(FakeRequest())

      val controllerApp = app.injector.instanceOf[HomeController]
      val homeApp = controllerApp.report().apply(FakeRequest())

      val requestLocalhost = FakeRequest(GET, "/report").withHeaders("Host" -> "localhost")
      val homeLocalhost = route(app, requestLocalhost).get

      val homes = List(home, homeApp, homeLocalhost)
      val allKeywords = headerKeywords ++ mainKeywords ++ reportKeywords ++ reportSegmentKeywords ++ paginationSegmentKeywords

      checkRoute(allKeywords, homes, false)
    }

    "render the error page from a new instance of controller, inside application and in localhost" in {
      val errorMessage : String = "test error message";
      val controller = new HomeController(new MockCacheApi)
      val home = controller.error(errorMessage).apply(FakeRequest())

      val controllerApp = app.injector.instanceOf[HomeController]
      val homeApp = controllerApp.error(errorMessage).apply(FakeRequest())

      val requestLocalhost = FakeRequest(GET, "/error?message=" + errorMessage).withHeaders("Host" -> "localhost")
      val homeLocalhost = route(app, requestLocalhost).get

      val homes = List(home, homeApp, homeLocalhost)

      checkRoute((errorKeywords :+ errorMessage), homes, false)
    }

    "render the reportPage page from a new instance of controller, inside application and in localhost" in {
      val page  = 1 to 9;
      forAll(page) {p =>
        val controller = new HomeController(new MockCacheApi)
        val home = controller.reportPage(p).apply(FakeRequest())

        val controllerApp = app.injector.instanceOf[HomeController]
        val homeApp = controllerApp.reportPage(p).apply(FakeRequest())

        val requestLocalhost = FakeRequest(GET, "/reportPage?pageNumber=" + p.toString).withHeaders("Host" -> "localhost")
        val homeLocalhost = route(app, requestLocalhost).get

        val homes = List(home, homeApp, homeLocalhost)

        val allKeywords = reportSegmentKeywords ++ paginationSegmentKeywords

        checkRoute(allKeywords, homes, false)
      }
    }

    "render the query page from a new instance of controller, inside application and in localhost" in {
      val validQueries = List("Italy", "IT", "Brazil", "BR", "Austria", "Canada");
      val wrongQueries = List("", "test", "error", "wtf")
      forAll(validQueries) { q =>
        val controller = new HomeController(new MockCacheApi)
        val home = controller.query(q, 1).apply(FakeRequest())

        val controllerApp = app.injector.instanceOf[HomeController]
        val homeApp = controllerApp.query(q, 1).apply(FakeRequest())

        val requestLocalhost = FakeRequest(GET, "/query?countryNameOrCode="+ q +"&pageNumber=1").withHeaders("Host" -> "localhost")
        val homeLocalhost = route(app, requestLocalhost).get

        val homes = List(home, homeApp, homeLocalhost)

        val allKeywords = querySegmentKeywords ++ paginationSegmentKeywords

        checkRoute(allKeywords, homes, false)
      }

      forAll(wrongQueries) { q =>
        val controller = new HomeController(new MockCacheApi)
        val home = controller.query(q, 1).apply(FakeRequest())

        val controllerApp = app.injector.instanceOf[HomeController]
        val homeApp = controllerApp.query(q, 1).apply(FakeRequest())

        val requestLocalhost = FakeRequest(GET, "/query?countryNameOrCode=" + q).withHeaders("Host" -> "localhost")
        val homeLocalhost = route(app, requestLocalhost).get

        val homes = List(home, homeApp, homeLocalhost)

        val allKeywords = querySegmentKeywords ++ paginationSegmentKeywords

        checkRoute(allKeywords, homes, true)
      }
    }
  }
}

package controllers

import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import org.scalatest.Inspectors.{forAll}
import org.squeryl.dsl._
import scala.util._

import play.api.cache._
import play.api.mvc._
import scala.concurrent._
import models._

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

  "Grouper" should {
    "groupCountryCodeAndSurfaces must group correctly the provided inputs" in {
      val countryAndAirportID = Try(
        List(("Italy", 1), ("Italy", 2), ("Italy", 3), ("Brazil", 4), ("Brazil", 5), ("Brazil", 6)
        )
      )

      val surfaceByAirportID : Try[List[org.squeryl.dsl.Group[Product2[Option[String],Int]]]] = Try(
        List(new Group(new Tuple2(None, 1)),
             new Group(new Tuple2(Some("testSurfaceIta1"), 1)),
             new Group(new Tuple2(Some("testSurfaceIta2"), 1)),
             new Group(new Tuple2(Some("testSurfaceIta3"), 1)),
             new Group(new Tuple2(Some("testSurfaceIta4"), 2)),
             new Group(new Tuple2(Some("testSurfaceIta5"), 2)),
             new Group(new Tuple2(Some("testSurfaceIta6"), 3)),
             new Group(new Tuple2(None, 3)),
             new Group(new Tuple2(Some("testSurfaceBrazil1"), 4)),
             new Group(new Tuple2(Some("testSurfaceBrazil2"), 4)),
             new Group(new Tuple2(Some("testSurfaceBrazil3"), 4)),
             new Group(new Tuple2(Some("testSurfaceBrazil4"), 5)),
             new Group(new Tuple2(Some("testSurfaceBrazil5"), 5)),
             new Group(new Tuple2(Some("testSurfaceBrazil6"), 6)),
             new Group(new Tuple2(None, 6))
        ))

      val expected = Try(
        Map( "Italy" -> List("testSurfaceIta1", "testSurfaceIta2", "testSurfaceIta3", "testSurfaceIta4", "testSurfaceIta5", "testSurfaceIta6"), "Brazil" -> List("testSurfaceBrazil1", "testSurfaceBrazil2", "testSurfaceBrazil3", "testSurfaceBrazil4", "testSurfaceBrazil5", "testSurfaceBrazil6")
        )
      )
      val result = Grouper.groupCountryCodeAndSurfaces(countryAndAirportID, surfaceByAirportID)
      result mustBe expected
    }

    "groupAirportsAndRunaways must group correctly the provided input" in {
      val airports = List(Airport(46181,"IT-0001","small_airport","PRATI NUOVI",45.268665f,7.947943f,Some(768),"IT"),
                          Airport(46546,"IT-0002","small_airport","Senigalia Airstrip",43.738266f,13.1832075f,None,"IT"),
                          Airport(46547,"IT-0003","small_airport","Field Sansepolcro",43.558502f,12.155771f,None,"IT"),
                          Airport(298337,"IT-0004","small_airport","Aviosuperficie Eremo della Giubiliana",36.861286f,14.627008f,Some(1401),"IT"))
      val runaways = List(Runaway(269408,6523,"00A",Some(80),Some(80),Some("ASPH-G"),true,false),
                         Runaway(255155,6524,"00AK",Some(2500),Some(70),Some("GRVL"),false,false),
                         Runaway(254165,6525,"00AL",Some(2300),Some(200),Some("TURF"),false,false),
                         Runaway(270932,6526,"00AR",Some(40),Some(40),Some("GRASS"),false,false)
                         )
      val expected = Map(Airport(46181,"IT-0001","small_airport","PRATI NUOVI",45.268665f,7.947943f,Some(768),"IT") -> List(Runaway(269408,6523,"00A",Some(80),Some(80),Some("ASPH-G"),true,false)),
                                 Airport(46546,"IT-0002","small_airport","Senigalia Airstrip",43.738266f,13.1832075f,None,"IT") -> List(Runaway(255155,6524,"00AK",Some(2500),Some(70),Some("GRVL"),false,false)),
                                 Airport(46547,"IT-0003","small_airport","Field Sansepolcro",43.558502f,12.155771f,None,"IT") -> List(Runaway(254165,6525,"00AL",Some(2300),Some(200),Some("TURF"),false,false)),
                                 Airport(298337,"IT-0004","small_airport","Aviosuperficie Eremo della Giubiliana",36.861286f,14.627008f,Some(1401),"IT") -> List(Runaway(270932,6526,"00AR",Some(40),Some(40),Some("GRASS"),false,false)))

      val result = Grouper.groupAirportsAndRunaways(airports, runaways)

      result mustBe expected
    }
  }
}

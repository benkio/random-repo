package models

import org.scalatestplus.play._
import org.scalatest.Assertions
import org.scalatest.Inspectors.{forAll}
import org.scalatest.prop._
import play.api.test._
import play.api.test.Helpers._
import scala.util._

import org.squeryl.PrimitiveTypeMode.inTransaction
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

import play.api.cache._
import play.api.mvc._
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._

import models._

class QueriesTest extends PlaySpec with OneAppPerSuite with MockitoSugar {

  implicit override lazy val app = FakeApplication(additionalConfiguration = inMemoryDatabase())

  val mockCacheAPI = mock[CacheApi]
  val database = new Database(mockCacheAPI)

  val countryCodeTestData = Map("IT" -> "Italy",
                                "US" -> "United States",
                                "AT" -> "Austria",
                                "BR" -> "Brazil",
                                "DE" -> "Germany",
                                "GR" -> "Greece",
                                "NL" -> "Netherlands",
                                "TR" -> "Turkey",
                                "UY" -> "Uruguay",
                                "ZA" -> "South Africa" )

  val countryCodeTestErrorData = Map ("" -> "",
                                      "test" -> "test",
                                      "sahetuaes" -> "henh")

  def testWithData(test : Tuple2[String, String] => Unit ) = {
    forAll(countryCodeTestData){ t => test(t) }
  }

  def testWithErrorData(test : Tuple2[String, String] => Unit ) = {
    forAll(countryCodeTestErrorData){ t => test(t) }
  }

  "CountryQueries" should {

    "countryByCodeOrName - return the expected countrycode" in {
      testWithData{ case (k, v) => {
          val result : String  = inTransaction { CountryQueries.countryByCodeOrName(v).single }
          result mustBe k
      }}
      testWithErrorData{ case (k,v) => {
        an [Exception] must be thrownBy {
          inTransaction { CountryQueries.countryByCodeOrName(v).single }
        }
      }}
    }

    "countries - return the expected record" in {
        val countries = inTransaction { CountryQueries.countries.toList }
        testWithData(e => (countries.map(c => (c.code,c.name))) must contain (e))
    }

    "countries with pagination - must return a limited number of elements " in {
      val countries = inTransaction { CountryQueries.countries(10, 0).toList }
      countries must have size 10
    }
  }

  "AirportQueries" should {
    val offset = 0
    val pageLength = 10

    "airportsByCountryCode method return a fixed number of results and has the same iso country" in {
      testWithData{ case (c, v) => {
                     val result : List[Airport] = inTransaction { AirportQueries.airportsByCountryCode(c,offset, pageLength).toList }

                     result.length must be <= pageLength
                     forAll(result.map(_.iso_country)) {code => code mustBe (c) }
      }}

      testWithErrorData{ case (c, v) => {
        val result = inTransaction { AirportQueries.airportsByCountryCode(c,offset, pageLength).toList }
        result.length mustBe 0
      }}
    }

    "getNumberOfAirportsByCountry return a number >=1" in {
      testWithData{ case (c, v) => {
                              val result = inTransaction { AirportQueries.getNumberOfAirportsByCountry(c).single }
                              result.measures must be >= 1L
                            }}
    }

    "getAirportDenseCountries method return a fixed number of results, has the same iso country and is sorted" in {
      testWithData{ case (c, v) => {
        val resultDesc : List[GroupWithMeasures[String, Long]] = inTransaction { AirportQueries.getAirportDenseCountries(pageLength, true).toList }
        val resultAsc : List[GroupWithMeasures[String, Long]] = inTransaction { AirportQueries.getAirportDenseCountries(pageLength, false).toList }

        resultAsc.length must be <= pageLength
        resultDesc.length must be <= pageLength
        (resultDesc.map(g => g.measures).reverse) mustBe sorted
        (resultAsc.map(g => g.measures)) mustBe sorted
      }}
    }

    "airportByCountry must contain only test data country codes " in {
      val result = inTransaction { AirportQueries.airportByCountry(countryCodeTestData.map(_._1).toList).toList.map(_._1) }
      testWithData(d =>{ (result) must contain (d._2) })
    }
  }

  "RunawayQueries " should {
    val inputAirports = List(Airport(6523,"00A","heliport","Total Rf Heliport",40.07080078125f,-74.93360137939453f,None, "US"),
                             Airport(6544,"00LL","heliport","Ac & R Components Heliport",39.66529846191406f,-89.70559692382812f,None,"IT"),
                             Airport(6813,"05CO","small_airport","Rancho De Aereo Airport",40.2149839f,-104.9844228f,Some(4978), "NL"))
/*
    "runawaysInAirport must return runaways with same id as list of airports" in {
      val result = inTransaction {
        println(inputAirports) //print the list properly
        val a = RunawayQueries.runawaysInAirport(inputAirports)
        println(a) PRINT THE QUERY WITHOUT THE WHERE CLAUSE!!!
      }
      val resultIdIdent = result.map(r => (r.airport_ref, r.airport_ident))
      val inputAirportsIdIdent = inputAirports.map(a => (a.id, a.ident))
      resultIdIdent must contain theSameElementsAs inputAirportsIdIdent
 }*/

    "runawaySurfaceAndAirportRef must have surface not null" in {
      val result = inTransaction { RunawayQueries.runawaySurfaceAndAirportRef.toList }
      val surfaces = result.map(_.key._1.isEmpty)
      forAll(surfaces) {v => v mustBe false}
    }
  }

  "Database " should {
    "countries must return a try and contain test data" in {
      val result = database.countries
      (result) mustBe a [Try[_]]
      result.isSuccess mustBe true
      testWithData( t => (result.get.map(c => (c.code, c.name))) must contain (t) )
    }
/*
    THE MOCK OBJECT OF THE CACHE THROWS A NULLPOINTER EXCEPTION
    "getAirportDenseCountries must return a try, contain at Least US and be sorted" in {
      val resultMost = database.getAirportDenseCountries(10, true)
      val resultLess = database.getAirportDenseCountries(10, false)

      println(resultMost)
      println(resultLess)

      (resultMost) mustBe a [Try[_]]
      (resultMost.isSuccess) mustBe true
      (resultMost.get.map(_._2).reverse) mustBe sorted
      (resultMost.get.map(_._1)) must contain ("United States")

      (resultLess) mustBe a [Try[_]]
      (resultLess.isSuccess) mustBe true
      (resultLess.get.map(_._2)) mustBe sorted

    }

  SAME PROBLEM OF THE WHERE CLAUSE
    """airportRunawayQuery:
         - result must be a Try[_]
         - airportCount must be greater than airports list size
         - airports must have size of 50
         - runaways must has airport_ref and airport_ident contained in airports id and indent""" in {
      testWithData{ case (c, v) => {
                     val result = database.airportRunawayQuery(v, 0, 50)

                     (result) mustBe a [Try[_]]
                     (result.isSuccess) mustBe true
                     (result.get._1) must have size 50
                     (result.get._1.length.toLong) must be <= (result.get._3)
                     forAll(result.get._2) { r => {
                                              (result.get._1.map(_.id)) must contain (r.airport_ref)
                                              (result.get._1.map(_.ident)) must contain (r.airport_ident)
                                            } }
                   }}
    }
 */
  }
}

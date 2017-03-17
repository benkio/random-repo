package models

import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._

import org.squeryl.PrimitiveTypeMode.inTransaction

import models._

class CountryQueriesTest extends PlaySpec {
  "CountryQueries" should {
    "return the expected countrycode" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val input = "Italy"
        val result : String  = inTransaction { CountryQueries.countryByCodeOrName(input).single }
        result mustBe "IT"
      }
    }
  }
}

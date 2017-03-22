package controllers

import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import org.scalatest.Inspectors.{forAll}
import org.scalatest.prop.GeneratorDrivenPropertyChecks //ScalaCheck
import controllers.utils._

class PaginationSpec extends PlaySpec {

  val paginationLengths = List (9,50,25,10,100,4)

  val pageNumbers = List( 10, 2, 3, 1, 22)
  val totalElements = List (247L, 5000L, 644909L, 5021L, 6742L, 21225L)

  "PaginationLogic" should {
    "return the pagination length encapsulated" in {
      forAll(paginationLengths) { paginationLength : Int =>
        val paginationLogic = new PaginationLogic(paginationLength)

        paginationLogic.paginationLength mustBe (paginationLength)
      }
    }
    "getOffset" in {
      forAll(paginationLengths) { paginationLength : Int =>
        val paginationLogic = new PaginationLogic(paginationLength)
        forAll(pageNumbers) { p =>
          paginationLogic.getOffset(p) mustBe ((p - 1) * paginationLength)

        }
      }
    }

    "getTotalPages" in {
      forAll(paginationLengths) { paginationLength : Int =>
        val paginationLogic = new PaginationLogic(paginationLength)
        forAll(totalElements) { p =>
          paginationLogic.getTotalPages(p) mustBe ((p.toInt/paginationLength))
        }
      }
    }
  }
}

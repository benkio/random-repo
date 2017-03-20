package controllers.utils

trait PaginationLogic {
  val paginationLength = 50

  def getOffset(pageNumber : Int) : Int

  def getTotalPages(totalRecord : Long) : Int
}

// Object with logic for the pagination, used in both controllers and views
object PaginationLogic extends PaginationLogic {

  def getOffset(pageNumber : Int) : Int = {
    (pageNumber - 1) * paginationLength
  }

  def getTotalPages(totalRecord : Long) : Int = {
    (totalRecord.toInt/paginationLength) + 1
  }
}

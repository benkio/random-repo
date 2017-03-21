package controllers.utils

trait IPaginationLogic {

  def paginationLength : Int

  def getOffset(pageNumber : Int) : Int

  def getTotalPages(totalRecord : Long) : Int
}

// Object with logic for the pagination, used in both controllers and views
class PaginationLogic(pageLength : Int) extends IPaginationLogic {

  def paginationLength = pageLength

  def getOffset(pageNumber : Int) : Int = {
    (pageNumber - 1) * paginationLength
  }

  def getTotalPages(totalRecord : Long) : Int = {
    (totalRecord.toInt/paginationLength)
  }
}

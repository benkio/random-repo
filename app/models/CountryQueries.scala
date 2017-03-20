package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable

object CountryQueries {
  import Database.countryTable

  def countryByCodeOrName(codeOrName : String) : Query[String] = from(countryTable) {
    country => where (country.code === codeOrName or country.name === codeOrName) select(country.code)
  }

  def countries : Query[Country] = from(countryTable) {
    country => select (country)
  }

  def countries(pageLength : Int, offset : Int) : Query[Country] = countries.page(offset, pageLength)

}

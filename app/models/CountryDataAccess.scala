package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table
import org.squeryl.Query
import collection.Iterable

object CountryDataAccess {
  import Database.countryTable

  def countryByCodeOrName(codeOrName : String) : Query[String] = from(countryTable) {
    country => where (country.code === codeOrName or country.name === codeOrName) select(country.code)
  }

}

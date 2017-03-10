# random-repo

Airports data from http://ourairports.com/data/ (released under public domain)

# Requirements 

1. There are 3 CSV files in the repository. The files contain data for countries, airports and runway information.

2. Write web application that will ask the user for two options - Query or Reports:

  1. Query Option will ask the user for the country name or code and print the airports & runways at each airport. The input can be country code or country name. For bonus points make the test partial/fuzzy. e.g. entering zimb will result in Zimbabwe :)

  2. Choosing Reports will print the following: 
    * 10 countries with highest number of airports (with count) and countries with lowest number of airports.
    * Type of runways (as indicated in "surface" column) per country
    * Bonus: Print the top 10 most common runway identifications (indicated in "le_ident" column) 

3. Write Tests

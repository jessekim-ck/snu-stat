library(readr)
library(dplyr)
library(RSQLite)
con <- dbConnect(SQLite(), "project.sqlite")


### Creating tables ###

airports <- read_csv(
  "./data/airports.csv", 
  col_names = c(
    "id", 
    "name", # airport name
    "city", # city
    "country", # country
    "IATA", # three-letter airport code
    "ICAO", # four-letter airport code
    "latitude", 
    "longitude",
    "MAMSL", # meters above mean sea level (in feets)
    "UTC", # timezone
    "continent", # continent code: A, E, N, O, S, U, Z
    "timezone", # timezone name
    "airport", # single value variable
    "OurAirports" # single value variable
  ),
  na = "\\N"
)
airlines <- read_csv(
  "./data/airlines.csv"
)
airplanes <- read_csv(
  "./data/airplanes.csv"
)

print("Adding Tables...\n")
DBI::dbWriteTable(con, "airports", airports, overwrite = TRUE)
DBI::dbWriteTable(con, "airlines", airlines, overwrite = TRUE)
DBI::dbWriteTable(con, "airplanes", airplanes, overwrite = TRUE)
print("Added Tables\n")

### Creating tables ###


### Creating table "flights" ###

for (year in 2001:2018) {
  for (month in 1:12) {
    data_path <- sprintf("./data/%d%02d.zip", year, month)
    cat("Adding data ", data_path, "...\n", sep = "")
    data <- read_csv(data_path)
    dbWriteTable(con, "flights", data, append = TRUE)
    cat("Added data ", data_path, "\n", sep = "")
  }
}

### Creating table "flights" ###


### Adding indices ###

col_names <- dplyr::tbl(con, "flights") %>% colnames

print("Adding index: Date\n")
dbSendQuery(con, "CREATE INDEX Date on flights(Year, Month, Day_of_Month)")
print("Added index: Date\n")

for (col in col_names) {
  cat("Adding index: ", col, "\n", sep = "")
  dbSendQuery(con, sprintf("CREATE INDEX %s on flights(%s)", col, col))
  cat("Added index: ", col, "\n", sep = "")
}

### Adding indices ###


dbDisconnect(con)



#' Produces a list of attendance/coordinates at each stadium that was used in the Finals
#'
#' @return A list of attendance/coordinates at each stadium that was used in the Finals
#'
#' @import dplyr
#'
#' @export
#'
getStadiumsInfo <- function(){

  dataSummarised <- getSummarised()

  year <- dataSummarised %>%
    select(Year)

  # Changes country name to work with countrycode function
  winner <- dataSummarised %>%
    mutate(Winner = ifelse(Winner == "England", "United Kingdom", Winner)) %>%
    mutate(Winner = ifelse(Winner == "Germany FR", "Germany", Winner)) %>%
    select(Winner)


  dataMatches <- getMatches()

  stadiums <- dataMatches %>%
    filter(Stage %in% "Final") %>%
    select(Stadium)

  attendance <- dataMatches %>%
    filter(Stage %in% "Final") %>%
    select(Attendance)

  # Coordinates found online corresponding to each stadium
  latitudes <- c(-34.894722, 41.929167, 48.840556, 46.962778, 59.370278,
                 -34.905556, 51.556021, 19.002778, 52.514722, -34.545278,
                 40.453056, 19.302778, 41.934167, 34.161389, 48.924722,
                 35.513056, 52.514722, -26.238611, -22.912167, -22.912167)

  longitudes <- c(-56.165833, 12.471944, 2.548611, 7.464444, 18.005556,
                  -56.191944, -0.279519, -99.150833, 13.239444, -58.449722,
                  -3.688333, -99.150833, 12.454722, -118.1675, 2.520556,
                  139.624444, 13.239444, 28.027528, -43.030556, -43.230556)

  dataframe <- data.frame(year, winner, stadiums, attendance, latitudes, longitudes)

  return(dataframe)
}



#' Helper function that gets country code
#'
#' @return
#'
#' @import dplyr
#' @import countrycode
#'
#' @export

getCountryCode <- function(countryname){

  data <- getStadiumsInfo()

  #Filters the country inputted
  store <- data %>%
    filter(Winner == countryname) %>%
    select(Winner)

  #Converts to string
  country <- paste(store)

  #Gets country code
  country_code <- countrycode::countrycode(country, "country.name", "iso2c")

  return(country_code)

}


#' Helper function that gets country flag
#'
#' @return
#'
#' @export

getFlagEmoji <- function(country_code) {
  emoji_code <- as.hexmode(utf8ToInt(country_code)) + 127397
  emoji_flag <- intToUtf8(as.integer(emoji_code), multiple = TRUE)
  return(paste0(emoji_flag, collapse = ""))

  #Refrenced Chat GPT
}



#' Produces leaflet plot with country flags
#'
#' @return a plot
#'
#' @import dplyr
#' @import leaflet
#' @import countrycode
#'
#' @export
#'

stadiumsMap <- function() {

  data <- getStadiumsInfo()

  # Creates leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)

  # Adds markers to each stadium
  for (i in 1:nrow(data)) {
    stadium_data <- data[i, ]

    # Calls country code function to get country coode
    country_code <- getCountryCode(stadium_data$Winner)

    # uses country code to generate country emoji flag
    flag_emoji <- getFlagEmoji(country_code)

    # Creates marker including stadium name, country name/flag, year, and attendance
    marker_info <- paste0(
      "<strong>", "Stadium Name: ", stadium_data$Stadium, "</strong><br>",
      "Winning Team: ", flag_emoji, " ", stadium_data$Winner, "<br>",
      "Year: ", stadium_data$Year, "<br>",
      "Stadium Attendance: ", stadium_data$Attendance, " People")

    # Add marker to map
    map <- map %>%
      addMarkers(lng = stadium_data$longitudes, lat = stadium_data$latitudes, popup = marker_info)
  }

  # Display the map
  return(map)
}

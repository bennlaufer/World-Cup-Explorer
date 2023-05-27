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
#' @return country code
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
#' @return country emoji
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

    # Calls country code function to get country code
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
      addMarkers(lng = stadium_data$longitudes,
                 lat = stadium_data$latitudes,
                 popup = marker_info)
  }

  # Display the map
  return(map)
}

#' Produces Bar Graph with Top Goal Scorers in WC Competitions
#'
#' @import tidyverse ggplot2
#'
#' @export
#'
#' @return a bar plot

topGoalScorers <- function(num = 7) {

  #making sure that num is within a reasonable range for the functionality of this graph
  if(num < 5 | num > 15) {
    stop("Number is not within the valid range (5 - 15)")
  }

  #grabbing the players data
  players <- getPlayers()

  #sorting out all unnecessary info and counting total # of goals per player
  playersGraph <- players |>
    select(Player.Name, Event) |>
    group_by(Player.Name) |>
    filter(!(Event %in% "")) |>
    mutate(GoalCount = str_count(Event, pattern = "G[:digit:]{2}'"),
           Player.Name = toupper(Player.Name)) |>
    summarise(GoalCount = sum(GoalCount)) |>
    arrange(desc(GoalCount)) |>
    slice_head(n = num)

  #Special characters which were improperly read in the dataset manually adjusted
  row_index <- which(playersGraph$Player.Name == "PEL� (EDSON ARANTES DO NASCIMENTO)")
  playersGraph[row_index,"Player.Name"] <- "PELÉ" #managing Pele's name length and accent

  row_index <- which(playersGraph$Player.Name == "M�LLER")
  playersGraph[row_index,"Player.Name"] <- "MÜLLER" #managing Muller's accent

  #graph creation
  graph <- playersGraph |>
    mutate(
      Player.Name = forcats::fct_reorder(Player.Name, GoalCount)
    ) |>
    ggplot(aes(x = GoalCount, y = Player.Name, label = GoalCount)) +
    geom_col(fill = "#256e35") +
    geom_text(color = "#256e35", nudge_x = .3) +
    theme(rect = element_rect(fill = "#EAD577"),
          text = element_text(color = "#242424"),

          panel.background = element_rect(fill = "#EAD577"),
          axis.text.x = element_text(size = 10, color = "#242424"),
          axis.text.y = element_text(size = 10, color = "#242424"),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(size = 15, vjust = 3),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#242424"),
          panel.grid.minor.x = element_line(color = "#242424")) +
    labs(
      title = "Top Goal Scorers in All WC Competitions (1930 - 2014)",
      y = "",
      x = "Total Goals Scored")

  #messing around with gganimate, didn't like the way it looked
  #   +
  #   transition_states(GoalCount, transition_length = 2, state_length = 1) +
  #   ease_aes('sine-in-out')
  #
  # anim <- animate(graph, nframes = 100)
  # anim_save("animated_wc_top_scorers.gif", anim)

  return(graph)

}


#' Produces Leaflet Map that shows how many goals each country has scored
#'
#' @import tidyverse leaflet
#'
#' @export
#'
#' @return a leaflet map of countries and their respective goals

countryGoalsMap <- function(){

  country_goals <- getCountryGoals()
  latlong <- getCountryLatLong()

  #combining the country goals data with the countries latitude and longitude
  country_info <- cbind(country_goals, latlong)

  #creating the information presented in each marker
  marker_info <- paste0(
    "<strong>", country_info$Country, "</strong><br>",
    "Total Goals Scored: ", country_info$Total_Goals, "<br>",
    "Goals Scored As Home Team: ", country_info$Home_Goals, "<br>",
    "Goals Scored As Away Team: ", country_info$Away_Goals)

  #creating leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    addMarkers(lat = country_info$latitude,
               lng = country_info$longitude,
               popup = marker_info)

  return(map)
}

#' Helper Function which returns the finalized dataframe of countries and goals they have scored in the worldcup
#'
#' @import tidyverse
#'
#' @export
#'
#' @return a dataframe

getCountryGoals <- function() {

  #getting matches data, omitting na row, and altering weird country formatting
  matches <- getMatches() |>
    mutate(Home.Team.Name = str_replace(Home.Team.Name, "rn\">", ""),
           Away.Team.Name = str_replace(Away.Team.Name, "rn\">", "")) |>
    na.omit()

  #Splitting up the datasets to be rejoined, separation under home and away goals
  matches_home <- matches |>
    select(Home.Team.Name, Home.Team.Goals) |>
    group_by(Home.Team.Name) |>
    summarise(Home_Goals = sum(Home.Team.Goals)) |>
    rename(Country = Home.Team.Name)


  matches_away <- matches |>
    select(Away.Team.Name, Away.Team.Goals) |>
    group_by(Away.Team.Name) |>
    summarise(Away_Goals = sum(Away.Team.Goals)) |>
    rename(Country = Away.Team.Name)

  #merging the two dfs
  total_goals_country <- merge(x = matches_away,
                               y = matches_home,
                               by = "Country",
                               all = TRUE)

  #changing NA values to 0 so that total goals can be computed
  total_goals_country <- replace(total_goals_country, is.na(total_goals_country), 0)

  #computing total goals
  country_goals <- total_goals_country |>
    mutate(Total_Goals = Away_Goals + Home_Goals)

  country_goals

  #handling Iran and Cote D'Ivoire edgecases
  country_goals[18, ] <- c("Côte d'Ivoire",8, 5, 13)

  country_goals[37, ] <- c("Iran", 6, 1, 7)
  country_goals <- country_goals[-38, ]

  country_goals <- country_goals |>
    mutate(Total_Goals = as.numeric(Total_Goals),
           Away_Goals = as.numeric(Away_Goals),
           Home_Goals = as.numeric(Home_Goals))

  return(country_goals)

}

#' Helper Function which returns the finalized dataframe of countries lat/long values
#'
#' @export
#'
#' @return a dataframe

getCountryLatLong <- function() {

  #chatgpt generated lat/long values after giving it the finalized list of countries

  latitude <- c(28.0339, -11.2027, -38.4161, -25.2744, 47.5162, 50.5039, -16.2902, 43.9159, -14.235, 42.7339,
                7.3697, 56.1304, -35.6751, 35.8617, 4.5709, 9.7489, 45.1, 7.539989, 21.5218, 49.8175,
                49.8175, 56.2639, NA, -1.8312, 26.8206, 13.7942, 52.3555, 46.6031, NA, 51.1657, NA, 7.9465,
                39.0742, 18.9712, 15.2, 47.1625, 32.4279, 33.2232, 31.0461, 41.8719, 18.1096, 36.2048, NA,
                35.9078, 29.3117, 23.6345, 31.7917, 52.1326, -40.9006, 9.082, 54.7877, 60.472, -23.4425, -9.1899,
                51.9194, 39.3999, 53.4129, 45.9432, 61.524, 23.8859, 56.4907, 14.4974, 44.0165, NA, 48.669,
                46.1512, -30.5595, NA, 40.4637, 60.1282, 46.8182, 8.6195, 10.6918, 33.8869, 38.9637, 48.3794, 23.4241,
                -32.5228, 37.0902, 52.1307, NA, NA)

  longitude <- c(1.6596, 17.8739, -63.6167, 133.7751, 14.5501, 4.4699, -63.5887, 17.6791, -51.9253, 25.4858,
                 12.3547, -106.3468, -71.543, 104.1954, -74.2973, -83.7534, 15.2, -5.54708, -77.7812, 15.473,
                 15.473, 9.5018, NA, -78.1834, 30.8025, -88.8965, -1.1743, 1.8883, NA, 10.4515, NA, -1.0232,
                 21.8243, -72.2852, -86.2419, 19.5033, 53.688, 43.6793, 34.8516, 12.5674, -77.2975, 138.2529, 127.5101,
                 127.7669, 47.4818, -102.5528, -7.0926, 5.2913, 174.886, 8.6753, -6.4923, 8.4689, -58.4438, -75.0152,
                 19.1451, -8.2245, -8.2439, 24.9668, 105.3188, 45.0792, -4.2026, -14.4524, 21.0059, NA, 19.699,
                 14.9955, 22.9375, NA, -3.7492, 18.6435, 8.2275, 0.8248, -61.2225, 9.5375, 35.2433, 31.1656, 53.8478,
                 -55.7658, -95.7129, -3.7837, NA, NA)

  latlong <- data.frame(latitude, longitude)

  return(latlong)

}


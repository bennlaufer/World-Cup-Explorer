
#' Returns which team won the world cup given year input
#'
#' @param year The year of the world cup
#'
#' @return A string
#'
#' @import dplyr
#'
#' @export

winningTeam <- function(year){
  if (!is.numeric(year)) {
    stop("Input variable must be numeric.")
    warning("Input variable is not numeric.")
  }

  valid_years <- seq(from = 1930, to = 2014, by = 4)

  yearCheck <- year
    if(!(yearCheck %in% valid_years)){
      stop(
        paste("World Cups only occur every 4 years starting at 1930.",
              getClosest(yearCheck, valid_years, "year", "years"))
      )
    }

  worldCups <- getSummarised()
  worldCups <- worldCups %>%
    filter(Year == year)

  return(worldCups$Winner)

}

#' Returns the element(s) in a vector that are closest to a value
#'
#' @param n The number
#' @param v The numeric vector
#' @param sigular The singular unit
#' @param plural The plural unit
#'
#' @return A string

getClosest <- function(n, v, singular, plural) {
  if (length(v) < 1) {
    warning("Vector v should not be empty")
  }

  diffs <- abs(v - n)
  minimumDiffs <- which(diffs == min(diffs))

  if (length(minimumDiffs) == 1) {
    message <- paste0("The closest ", singular, " is ", v[minimumDiffs], ".")
  } else if (length(minimumDiffs) == 2) {
    message <- paste0("The closest ", plural, " are ", v[minimumDiffs[1]],
                      " and ", v[minimumDiffs[2]], ".")
  }

  return(message)
}

#' Returns the years won for a given country input
#'
#' @param country A country at the world cup
#'
#' @return dataframe of numeric values
#'
#' @import dplyr
#'
#' @export

yearsWon <- function(country){

  if (!is.character(country)) {
    stop("Input variable must be character.")
    warning("Input variable is not character.")
  }

  worldCups <- getSummarised()
  worldCups <- worldCups %>%
    filter(Winner == {{country}})

  return(worldCups$Year)

}

#' Returns the information for a given player name
#'
#'@param name A player's name at the world cup
#'
#' @return dataframe with numeric/char information about player
#'
#'
#' @import dplyr

#' @export

getPlayerInfo <- function(name){

  if (!is.character(name)) {
    stop("Input variable must be character.")
    warning("Input variable is not character.")
  }

  worldCups <- getPlayers()
  worldCups <- worldCups %>%
    filter(Player.Name == {{name}})

  return(worldCups)

}

#' Returns the information for a given match in the world cup
#'
#'@param id id of the world cup match
#'
#'@return string with information about the specific match
#'
#' @import dplyr
#'
#' @export

getMatchInfo <- function(id) {

  if (!is.numeric(id)) {
    stop("Input variable must be numeric.")
    warning("Input variable is not a number.")
  }

  matches <- getMatches()
  matches <- matches |>
    filter(MatchID == id)

  matchInfo <- paste(
    matches$Home.Team.Name[1], " vs. ", matches$Away.Team.Name[1], "\n",

    "The score was ", matches$Home.Team.Initials[1], " ", matches$Home.Team.Goals[1], " to ",

    matches$Away.Team.Goals[1], matches$Away.Team.Initials[1], "\n",

    "The game was held in ", matches$City, "in the ", matches$Stadium, " stadium on ", matches$Datetime, "\n",

    "The referee this game was ", matches$Referee, " with their assistants ",

    matches$Assistant.1, " and ", matches$Assistant.2
    )

  return(glue::glue(matchInfo))

}

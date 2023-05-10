
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

  yearCheck <- year
    if(yearCheck %% 1 != .5){
      stop("World Cups only occur every 4 years starting at 1930")
    }

  worldCups <- getSummarised()
  worldCups <- worldCups %>%
    filter(Year == year)

  return(worldCups$Winner)

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

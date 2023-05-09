
#' Returns which team won the world cup given year input
#'
#' @return A string
#' @param year The year of the world cup
#' @export

winningTeam <- function(year){
  if (!is.numeric(year)) {
    stop("Input variable must be numeric.")
    warning("Input variable is not numeric.")
  }

  WCList <- tidyWorldCup()
  worldCups <- WCLIst[[3]]
  worldCups <- worldCups %>%
    filter(Year == year)

  return(worldCups[Country])

}

#' Returns the years won for a given country input
#'
#' @return dataframe of numeric values
#' @param country A country at the world cup
#' @export

yearsWon <- function(country){

  if (!is.character(country)) {
    stop("Input variable must be character.")
    warning("Input variable is not character.")
  }

  WCList <- tidyWorldCup()
  worldCups <- WCList[[3]]
  worldCups <- worldCups %>%
    filter(country == {{country}})

  return(worldCups[Year])

}

#' Returns the information for a given player name
#'
#' @return dataframe with numeric/char information about player
#' @param name A player at the world cup
#' @export

getPlayerInfo <- function(name){

  if (!is.character(name)) {
    stop("Input variable must be character.")
    warning("Input variable is not character.")
  }
  WCList <- tidyWorldCup()
  worldCups <- WCList[[2]]
  worldCups <- worldCups %>%
    filter(worldCups[7] == {{name}})

  return(worldCups)

}

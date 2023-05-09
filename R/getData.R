#' Produces a list of tidy data frames of world cup data for the user
#'
#' @return A list of data frames
#'
#' @export
tidyWorldCup <- function() { #produces a list of different WC data frames for the user to use for other functions in the package

  data_dir_matches <- system.file("extdata","WorldCupMatches.csv", package = "rpack")
  data_dir_players <- system.file("extdata","WorldCupPlayers.csv", package = "rpack")
  data_dir_cups <- system.file("extdata","WorldCups.csv", package = "rpack")
  worldCupData <- list(utils::read.csv(data_dir_matches),
                       utils::read.csv(data_dir_players),
                       utils::read.csv(data_dir_cups))

  # "Today Nils Learned..." how R compresses data upon package installation
  # Very annoying and tedious process of learning directory naming schemes and how much confusion it can cause if
  # you have an improperly labeled directory

  return(worldCupData)

}

#' Gets the Matches data frame from the list
#'
#' @return A data frame
#'
#' @export
getMatches <- function() {

  WCList <- tidyWorldCup()
  matches <- WCList[[1]]

  return(matches)

}

#' Gets the Players data frame from the list
#'
#' @return A data frame
#'
#' @export
getPlayers <- function() {

  WCList <- tidyWorldCup()
  players <- WCList[[2]]

  return(players)

}

#' Gets the summarized world cup data frame from the list
#'
#' @return A data frame
#'
#' @export
getSummarised <- function() {

  WCList <- tidyWorldCup()
  summarised <- WCList[[3]]

  return(summarised)

}

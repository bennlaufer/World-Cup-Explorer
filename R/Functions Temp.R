#' Produces a list of tidy data frames of world cup data for the user
#'
#' @return A data Frame
#'
#' @export
tidyWorldCup <- function() { #produces a list of different WC data frames for the user to use for other functions in the package

  data_dir_matches <- system.file("extdata","WorldCupMatches.csv", package = "rpack")
  data_dir_players <- system.file("extdata","WorldCupPlayers.csv", package = "rpack")
  data_dir_cups <- system.file("extdata","WorldCups.csv", package = "rpack")
  worldCupData <- list(utils::read.csv(data_dir_matches),
                       utils::read.csv(data_dir_players),
                       utils::read.csv(data_dir_cups)) # "Today Nils Learned..." how R compresses data upon package installation

  return(worldCupData)

}

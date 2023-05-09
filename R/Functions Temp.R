#' Produces a tidy dataframe of world cup data for the user
#'
#' @return A data Frame
#'
#' @importFrom readr read.csv()
#'
#' @export


tidyWorldCup <- function() { #produces a list of different WC data frames for the user to use for other functions in the package

  data_dir <- system.file("data", package = "rpack")
  worldCupData <- readRDS(file.path(data_dir, "Rdata.rds")) # "Today Nils Learned..." how R compresses data upon package installation

  return(worldCupData)

}

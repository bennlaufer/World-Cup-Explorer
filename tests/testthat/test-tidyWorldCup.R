
test_that("tidyWorldCup works", {

  data_dir_matches <- system.file("extdata","WorldCupMatches.csv", package = "rpack")
  data_dir_players <- system.file("extdata","WorldCupPlayers.csv", package = "rpack")
  data_dir_cups <- system.file("extdata","WorldCups.csv", package = "rpack")
  worldCupData <- list(utils::read.csv(data_dir_matches),
                       utils::read.csv(data_dir_players),
                       utils::read.csv(data_dir_cups))

  correct_result <- worldCupData

  my_result <- tidyWorldCup()

  expect_equal(my_result, correct_result)

})

test_that("getMatches works", {

  data_dir <- system.file("extdata","WorldCupMatches.csv", package = "rpack")
  correct_result <- utils::read.csv(data_dir)

  my_result <- getMatches()

  expect_equal(my_result, correct_result)

})

test_that("getPlayers works", {

  data_dir <- system.file("extdata","WorldCupPlayers.csv", package = "rpack")
  correct_result <- utils::read.csv(data_dir)

  my_result <- getPlayers()

  expect_equal(my_result, correct_result)

})

test_that("getSummarised works", {

  data_dir <- system.file("extdata","WorldCups.csv", package = "rpack")
  correct_result <- utils::read.csv(data_dir)

  my_result <- getSummarised()

  expect_equal(my_result, correct_result)

})

test_that("winningTeam works", {

  correct_result <- "Germany"
  my_result <- winningTeam(2014)

  expect_equal(my_result, correct_result)


})

test_that("yearsWon works", {

  correct_result <- 2014
  my_result <- yearsWon("Germany")

  expect_equal(my_result, correct_result)


})

test_that("getPlayerInfo works", {

  correct_result <- getPlayers() %>%
    filter(Player.Name == "MESSI")

  my_result <- getPlayerInfo("MESSI")

  expect_equal(my_result, correct_result)


})

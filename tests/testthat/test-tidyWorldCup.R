
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
  matches <- utils::read.csv(data_dir)
  uniqueMatches <- matches[!duplicated(matches),]
  correct_result <- uniqueMatches[complete.cases(uniqueMatches),]

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

test_that("getMatchInfo works", {

  my_result <- getMatchInfo(1096)

  matches <- getMatches()
  matches <- matches |>
    filter(MatchID == 1096)

  matchInfo <- paste(
    matches$Home.Team.Name[1], " vs. ", matches$Away.Team.Name[1], "\n",

    "The score was ", matches$Home.Team.Initials[1], " ", matches$Home.Team.Goals[1], " to ",

    matches$Away.Team.Goals[1], matches$Away.Team.Initials[1], "\n",

    "The game was held in ", matches$City, "in the ", matches$Stadium, " stadium on ", matches$Datetime, "\n",

    "The referee this game was ", matches$Referee, " with their assistants ",

    matches$Assistant.1, " and ", matches$Assistant.2)

  correct_result <- glue::glue(matchInfo)

  expect_equal(my_result, correct_result)

  })

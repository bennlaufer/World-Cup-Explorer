test_that("topGoalScorerers catches input error", {

  expect_error(topGoalScorers(19), "Number is not within the valid range (5 - 15)", ignore.case = TRUE)

  }) #considering this successful

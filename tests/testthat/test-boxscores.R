library(dplyr)

poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

test_that("pitcher_box_score returns expected dataframe", {
  pitcher_data <- poly_utah_game %>%
    filter(PitcherId == 823513)

  # Expected result
  expected_box <- data.frame(IP=1.2, H=5, R=2, BB=2, K=0, row.names = "Dakoda West")

  # Actual result
  actual_box <- pitcher_box_score(data = poly_utah_game,
                               pitcherid = 823513)

  # Compare results
  expect_identical(expected_box, actual_box)
})

test_that("pitcher_box_score returns correct error from check_pitcherid given wrong id", {

  expect_error(pitcher_box_score(data = poly_utah_game, pitcherid = 1000),
               "pitcherid is not present as a pitcher in this game")

})

test_that("pitcher_box_score returns correct error from check_pitcherid with non-numeric input", {

  expect_error(pitcher_box_score(data = poly_utah_game, pitcherid = "Aaron Burr"),
               "pitcherid must be numeric.")

})

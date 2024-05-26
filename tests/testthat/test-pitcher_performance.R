poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))
library(dplyr)


test_that("pitcher_chart returns expected plot", {

})


test_that("movement_chart returns expected plot", {

})

test_that("release_chart returns expected plot", {

})

test_that("location_chart returns expected plot", {

})

test_that("pitcher_chart returns correct error from check_pitcherid", {

  expect_error(pitcher_chart(data = poly_utah_game,pitcherid = 1000),
               "pitcherid is not present as a pitcher in this game")

})

test_that("pitcher_chart returns correct error from check_pitcherid with non-numeric input", {

  expect_error(pitcher_chart(data = poly_utah_game, pitcherid = "Aaron Burr"),
               "pitcherid must be numeric.")

})

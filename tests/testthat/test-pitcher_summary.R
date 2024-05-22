poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))
library(dplyr)

test_that("pitcher_summary dimensions work", {
  correct_result <- c(7, 4)

  my_result <- pitcher_summary(data = poly_utah_game, pitcherid = 1000114562) %>%
    dim()

  expect_equal(my_result, correct_result)
})


test_that("pitcher_summary changing type works", {
  correct_result <- c(7, 6)

  my_result <- pitcher_summary(data = poly_utah_game,
                               pitcherid = 1000114562,
                               type = "auto") %>%
    dim()

  expect_equal(my_result, correct_result)
})

test_that("pitcher_summary works", {
  correct_result <- poly_utah_game %>%
    filter(PitcherId == 1000114562) %>%
    pull(RelSpeed) %>%
    mean() %>%
    round(digits = 4)

  my_result <- pitcher_summary(data = poly_utah_game,
                               pitcherid = 1000114562,
                               type = "tagged") %>%
    filter(Statistic == "Avg Velocity") %>%
    select(Overall) %>%
    pull() %>%
    round(digits = 4)

  expect_equal(my_result, correct_result)
})

test_that("pitcher_summary stats works", {

  cleaned_data <- poly_utah_game %>%
    filter(PitcherId == 1000114562) %>%
    mutate(PitchCallClass = case_when(
      PitchCall  %in% c("BallCalled", "HitByPitch") ~ "Ball",
      TRUE ~ "Strike")) %>%
    select(contains("Pitch"), RelSpeed, SpinRate, InducedVertBreak, Extension)

  correct_result <- cleaned_data %>%
    group_by(TaggedPitchType) %>%
    summarize(sum(PitchCallClass == "Strike")*100 / n()) %>%
    filter(TaggedPitchType == "Slider") %>%
    pull() %>%
    round(digits = 3)

  my_result <- pitcher_summary(data = poly_utah_game, pitcherid = 1000114562) %>%
    filter(Statistic == "Strike-Ball Percent") %>%
    select(Slider) %>%
    pull() %>%
    as.numeric() %>%
    round(digits = 3)

  expect_equal(my_result, correct_result)
})

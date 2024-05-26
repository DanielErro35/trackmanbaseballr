poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))
library(dplyr)


test_that("pitcher_chart returns expected plot", {
  pitcher_data <- poly_utah_game %>%
    filter(PitcherId == 1000114562)

  # Expected result
  expected_plot <- pitcher_data %>%
    ggplot(aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    xlab("Horizontal Break (in)") +
    ylab("Induced Vertical Break (in)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-30, 30) +
    ylim(-30, 30) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    theme_minimal()

  # Actual result
  actual_plot <- pitcher_chart(data = poly_utah_game,
                               pitcherid = 1000114562,
                               type = "movement")

  # Compare plots
  expect_identical(actual_plot$data, expected_plot$data)
  expect_equal(length(actual_plot$layers), length(expected_plot$layers))
})


test_that("movement_chart returns expected plot", {

})

test_that("release_chart returns expected plot", {

})

test_that("location_chart returns expected plot", {

})

test_that("pitcher_chart returns correct error with wrong graph type input", {

  error_message <- "Please input a valid chart type (\"movement\", \"release\", \"location\")"
  expect_error(pitcher_chart(data = poly_utah_game,
                             pitcherid = 1000114562,
                             type = "boxplot"),
               error_message)
})

test_that("pitcher_chart returns correct error from check_pitcherid given wrong id", {

  expect_error(pitcher_chart(data = poly_utah_game, pitcherid = 1000),
               "pitcherid is not present as a pitcher in this game")

})

test_that("pitcher_chart returns correct error from check_pitcherid with non-numeric input", {

  expect_error(pitcher_chart(data = poly_utah_game, pitcherid = "Aaron Burr"),
               "pitcherid must be numeric.")

})

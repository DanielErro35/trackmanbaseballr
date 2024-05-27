library(dplyr)
library(png)
poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

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
  pitcher_data <- poly_utah_game %>%
    filter(PitcherId == 823504)

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
  actual_plot <- movement_chart(data = pitcher_data,
                                firstname = "Evan",
                                lastname = "Vasiliou",
                                game_date = "5/10/2024")

  # Compare plots
  expect_identical(actual_plot$data, expected_plot$data)
  expect_equal(length(actual_plot$layers), length(expected_plot$layers))
})


test_that("release_chart returns expected plot", {
  pitcher_data <- poly_utah_game %>%
    filter(PitcherId == 809938)

  # Expected result
  expected_plot <-pitcher_data %>%
    ggplot(aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    xlab("Release Side (ft)") +
    ylab("Release Height (ft)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-4, 4) +
    ylim(0, 8) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    theme_minimal()

  # Actual result
  actual_plot <- release_chart(data = pitcher_data,
                               firstname = "Ryan",
                               lastname = "Baum",
                               game_date = "5/10/2024")

  # Compare plots
  expect_identical(actual_plot$data, expected_plot$data)
  expect_equal(length(actual_plot$layers), length(expected_plot$layers))
})

test_that("location_chart returns expected plot", {
  pitcher_data <- poly_utah_game %>%
    filter(PitcherId == 809938)

  # import strike zone image
  strike_zone <- readPNG(here::here("strikezone.png"))

  # Expected result
  expected_plot <- pitcher_data %>%
    ggplot(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    annotation_raster(strike_zone, -0.75, 0.75, 0, 4.5) +
    geom_point(stat = "identity", size = 3) +
    xlab("Horizontal Location (ft)") +
    ylab("Vertical Location (ft)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-3, 3) +
    ylim(0, 5)  +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  # Actual result
  actual_plot <- location_chart(data = pitcher_data,
                                firstname = "Ryan",
                                lastname = "Baum",
                                game_date = "5/10/2024")

  # Compare plots
  expect_identical(actual_plot$data, expected_plot$data)
  expect_equal(length(actual_plot$layers), length(expected_plot$layers))
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

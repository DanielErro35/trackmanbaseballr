#' Function to summarize a pitcher's game statistics
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#' @param pitch_type variable name "tagged" or "auto"
#'
#' @return A table indicating the percentage breakdown of types of pitches for
#' the pitcher
#'
#' @importFrom package function
#' @import dplyr
#'
#' @export
pitch_breakdown <- function(data, pitcherid, type = "tagged") {

  # check that pitcherid is a number
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game")
  }

  if (type == "tagged"){
    column = "TaggedPitchType"
  } else if (type == "auto"){
    column = "AutoPitchType"
  } else {
    stop("Pitch type must be 'tagged' or 'auto'")
  }

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid) %>%
    mutate(PitchCallClass = case_when(
      PitchCall  %in% c("BallCalled", "HitByPitch") ~ "Ball",
      TRUE ~ "Strike")) %>%
    select(contains("Pitch"), RelSpeed, SpinRate, InducedVertBreak, Extension)

  grouped_summary <- pitcher_data %>%
    group_by(TaggedPitchType) %>%
    pitch_summary()

  grouped_summary$Overall <- c(100, pitch_summary(pitcher_data))

  return(grouped_summary)

}

library(dplyr)
poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

pitch_breakdown(data = poly_utah_game, pitcherid = 1000114562)


#' Helper function to summarize columns of interest
#'
#' @param pitcher_data A trackman baseball dataframe, subsetted by pitcher of
#' interest with PitchCallClass column to calculate strikes
#'
#' @return A dataframe with pitch type as columns and summarized values as rows
pitch_summary <- function(pitcher_data) {

  pitcher_summarized <- pitcher_data %>%
    summarize(
      pitch_type_percent = n() / nrow(pitcher_data)*100,
      avg_velocity = mean(RelSpeed),
      avg_spinrate = mean(SpinRate),
      avg_induced_ver_break = mean(InducedVertBreak),
      avg_extension = mean(Extension),
      strike_ball_percent = sum(PitchCallClass == "Strike")*100 / n()
    ) %>%
    t() %>%
    as.data.frame()

  # Set new column names
  colnames(pitcher_summarized) <- pitcher_summarized[1, ]
  # Remove the first row
  pitcher_summarized <- pitcher_summarized[-1, ]

  return(pitcher_summarized)
}

#' Description of function
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#' @param pitch_type variable name "tagged" or "auto"
#'
#' @return A table indicating the percentage breakdown of types of pitches for
#' the pitcher
#'
#' @importFrom package function
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

  pitcher_data %>%
    group_by(!!sym(column)) %>%
    summarize(
      pitch_type_percent = n() / nrow(pitcher_data)*100,
      avg_velocity = mean(RelSpeed),
      avg_spinrate = mean(SpinRate),
      avg_induced_ver_break = mean(InducedVertBreak),
      avg_extension = mean(Extension),
      strike_ball_percent = sum(PitchCallClass == "Strike")*100 / n()
    )

}

library(dplyr)
poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

pitch_breakdown(data = poly_utah_game, pitcherid = 1000114562)


# besides just % of each type of pitch, summarize
# do not calculate: proportion strikes, balls, fouls, hits (hits can be broken down further)
# strike-to-ball-percentage (balls, hit by pitch vs everything else)
# average speed

#' Description for helper function
#'
#' @param name description
#' @param name description
#'
#' @return

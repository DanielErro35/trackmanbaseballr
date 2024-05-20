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

  pitcher_summarized <- pitcher_data %>%
    group_by(!!sym(column)) %>%
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

library(dplyr)
poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

pitch_breakdown(data = poly_utah_game, pitcherid = 1000114562)


### Test function
pitcher_data <- poly_utah_game %>%
  filter(PitcherId == 1000114562) %>%
  mutate(PitchCallClass = case_when(
    PitchCall %in% c("BallCalled", "HitByPitch") ~ "Ball",
    TRUE ~ "Strike")) %>%
  select(contains("Pitch"), RelSpeed, SpinRate, InducedVertBreak, Extension)

pitcher_data %>%
  group_by(TaggedPitchType) %>%
  summarize(
    pitch_type_percent = n() / nrow(pitcher_data)*100,
    avg_velocity = mean(RelSpeed),
    avg_spinrate = mean(SpinRate),
    avg_induced_ver_break = mean(InducedVertBreak),
    avg_extension = mean(Extension),
    strike_ball_percent = sum(PitchCallClass == "Strike")*100 / n()
  )


pitch_data <- tibble(
  pitch_type = c("Fastball", "Slider", "Curveball", "Changeup", "Fastball"),
  pitch_call = c("Strike", "Ball", "Strike", "Strike", "Ball"),
  velocity = c(95, 86, 80, 88, 96),
  spin_rate = c(2400, 2200, 2500, 2100, 2450)
)

# with pitch_data, how do I summarize the proportion pitch type as well as pitch call, average velocity, and average spin rate by pitch type?
# streamline to use fewer summarize calls
pitch_summarized <- data.frame(
  pitch_type = c("Fastball", "Slider"),
  pitch_percent = c(83, 25),
  avg_velocity = c(95, 82),
  avg_spin = c(2052, 2242)
)

pitch_summarized_transposed <- pitch_summarized %>%
  t() %>%
  as.data.frame()

# Set new column names
colnames(pitch_summarized_transposed) <- pitch_summarized_transposed[1, ]
# Remove the first row
pitch_summarized_transposed <- pitch_summarized_transposed[-1, ]

# Set the row names to NULL to remove them
#row.names(pitch_summarized_transposed) <- NULL

pitch_summarized  %>%
  pivot_longer(cols = -pitch_type,
               names_to = "metric",
               values_to = "value")

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

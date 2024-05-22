#' Function to summarize a pitcher's game statistics
#'
#' @param data A trackman baseball dataset
#' @param pitcherid The ID of pitcher
#' @param pitch_type variable name "tagged" or "auto"
#'
#' @return A table indicating the percentage breakdown of types of pitches for
#' the pitcher
#'
#' @import dplyr
#'
#' @export
pitcher_summary <- function(data, pitcherid, type = "tagged") {

  # Check that pitcherid is a number in the dataset
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game.")
  }

  # Use specified pitch type column
  if (type == "tagged"){
    column = "TaggedPitchType"
  } else if (type == "auto"){
    column = "AutoPitchType"
  } else {
    stop("Pitch type must be 'tagged' or 'auto'")
  }

  # Clean and filter trackman data
  pitcher_data <- data %>%
    filter(PitcherId == pitcherid) %>%
    mutate(PitchCallClass = case_when(
      PitchCall  %in% c("BallCalled", "HitByPitch") ~ "Ball",
      TRUE ~ "Strike")) %>%
    select(contains("Pitch"), RelSpeed, SpinRate, InducedVertBreak, Extension)

  # Pitcher summary grouped by pitch type
  grouped_summary <- pitcher_data %>%
    group_by(!!sym(column)) %>%
    get_pitching_summary()

  # Add on overall pitcher game summary
  grouped_summary$Overall <- c(100, get_pitching_summary(pitcher_data))

  tibble::rownames_to_column(grouped_summary, var = "Statistic")

}


#' Helper function to summarize columns of interest
#'
#' @param pitcher_data A trackman baseball dataframe, subsetted by pitcher of
#' interest with PitchCallClass column to calculate strikes
#'
#' @return A dataframe with pitch type as columns and summarized values as rows
get_pitching_summary <- function(pitcher_data) {

  # get pitcher summary statistics
  pitcher_summarized <- pitcher_data %>%
    summarize(
      `Pitch Type Percent` = n() / nrow(pitcher_data)*100,
      `Avg Velocity` = mean(RelSpeed),
      `Avg Spin Rate` = mean(SpinRate),
      `Avg Induced Vert. Break` = mean(InducedVertBreak),
      `Avg Extension` = mean(Extension),
      `Strike-Ball Percent` = sum(PitchCallClass == "Strike")*100 / n()
    ) %>%
    t() %>%
    as.data.frame()

  # Set new column names
  colnames(pitcher_summarized) <- pitcher_summarized[1, ]
  # Remove the first row
  pitcher_summarized <- pitcher_summarized[-1, ]

  return(pitcher_summarized)
}


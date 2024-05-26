#' Function to summarize a pitcher's performance in a baseball game
#'
#' @param data A trackman baseball dataset
#' @param pitcherid The numeric ID of the pitcher to summarize
#'
#' @return A dataframe summarizing pitcher performance for a specified pitcher
#' in one baseball game, counting up innings, strikeouts, walks, and runs.
#'
#' @import dplyr
#'
#' @export
pitcher_performance <- function(data, pitcherid) {

  # Check that pitcherid is a number in the dataset
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game.")
  }

  # Clean and filter trackman data
  pitcher_data <- data %>%
    filter(PitcherId == pitcherid) %>%
    select(Inning, Outs, Balls, Strikes, RunsScored)


}

#' Helper function description
#'
#' @param name description
#'
#' @return
helper_function <- function(pitcher_data) {

}

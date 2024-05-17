#' Description of function
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A table indicating the percentage breakdown of types of pitches for
#' the pitcher
#'
#' @importFrom package function
#'
#' @export
movement_chart <- function(data, pitcherid){

  # check that pitcherid is a number
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game")
  }

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid)



}

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

  if (type == "tagged"){
    column = "TaggedPitchType"
  } else{
    column = "AutoPitchType"
  }

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid)

  pitcher_data %>%
    group_by(column) %>%
    summarize(percentage = n() / nrow(pitcher_data) * 100)


}



#' Description for helper function
#'
#' @param name description
#' @param name description
#'
#' @return

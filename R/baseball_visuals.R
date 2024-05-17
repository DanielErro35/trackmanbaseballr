#' Description of function
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A scatter plot displaying pitch movement profiles by pitch type
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

  # Split the pitcher's name from "Last, First" to "First" "Last"
  pitcher_name <- pitcher_data$Pitcher[1]
  name_parts <- strsplit(pitcher_name, ", ")[[1]]
  pitcher_lastname <- name_parts[1]
  pitcher_firstname <- name_parts[2]

  date <- pitcher_data$Date[1]

  pitcher_data %>%
    ggplot(aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    ggtitle(glue::glue("{pitcher_name}: Movement Profile ({date})")) +
    xlab("Horizontal Break") +
    ylab("Induced Vertical Break") +
    xlim(-30, 30) +
    ylim(-30, 30) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0)

}

#' Description of function
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A scatter plot displaying pitch movement profiles by pitch type
#'
#' @importFrom package function
#'
#' @export
release_point_chart <- function(data, pitcherid){

  # check that pitcherid is a number
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game")
  }

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid)

  # Split the pitcher's name from "Last, First" to "First" "Last"
  pitcher_name <- pitcher_data$Pitcher[1]
  name_parts <- strsplit(pitcher_name, ", ")[[1]]
  pitcher_lastname <- name_parts[1]
  pitcher_firstname <- name_parts[2]

  date <- pitcher_data$Date[1]

  pitcher_data %>%
    ggplot(aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    ggtitle(glue::glue("{pitcher_firstname} {pitcher_lastname}: Release Point ({date})")) +
    xlab("Release Side") +
    ylab("Release Height") +
    xlim(-4, 4) +
    ylim(0, 8) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0)

}

#' Function to provide a pitcher's box score from Trackman data
#'
#' @param data A trackman baseball dataset
#' @param pitcherid The numeric ID of the pitcher to summarize
#'
#' @return A box score table
#'
#' @import dplyr
#'
#' @export
pitcher_box_score <- function(data, pitcherid){

  # check that pitcherid is a number
  check_pitcherid(data, pitcherid)

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid)

  # Retrieve pitcher name
  name_parts <- split_pitcher_name(pitcher_data)
  pitcher_lastname <- name_parts[1]
  pitcher_firstname <- name_parts[2]

  # Calculates the number of hits the pitcher allowed
  pitcher_hits <- length(which(pitcher_data$PlayResult == "Single" |
                                 pitcher_data$PlayResult == "Double" |
                                 pitcher_data$PlayResult == "Triple" |
                                 pitcher_data$PlayResult == "HomeRun"))

  # Calculates the number of runs the pitcher allowed
  pitcher_runs <- sum(pitcher_data$RunsScored)

  # Calculates the number of batters the pitcher walked
  pitcher_walks <- length(which(pitcher_data$KorBB == "Walk"))

  # Calculates the number of batters the pitcher struck out
  pitcher_strikeouts <- length(which(pitcher_data$KorBB == "Strikeout"))

  # Calculates the number of innings pitched
  pitcher_innings <- round((sum(pitcher_data$OutsOnPlay) + pitcher_strikeouts)/3, digits = 1)

  # Reformatting pitcher_innings so that .1 and .2 represent 1/3 and 2/3 of an inning
  if (grepl("\\.3$", pitcher_innings)) {
    pitcher_innings <- as.numeric(sub("\\.3$", ".1", pitcher_innings))
  } else if (grepl("\\.7$", pitcher_innings)){
    pitcher_innings <- as.numeric(sub("\\.7$", ".2", pitcher_innings))
  }

  # Create box score dataframe, rename columns
  pitcher_box <- t(as.data.frame(c(pitcher_innings, pitcher_hits, pitcher_runs, pitcher_walks, pitcher_strikeouts),
                                 row.names = c("IP", "H", "R", "BB", "K")))

  # Replace dataframe row with pitcher name
  rownames(pitcher_box) <- c(glue::glue("{pitcher_firstname} {pitcher_lastname}"))


  return(pitcher_box)

}


#' Helper function that checks whether the pitcherid provided is valid in the given dataset
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return Stop if pitcherid is not valid
check_pitcherid <- function(data, pitcherid){

  # check that pitcherid is a number
  if (!is.numeric(pitcherid)) {
    stop("pitcherid must be numeric.")
  }  else if (!(pitcherid %in% data$PitcherId)) {
    stop("pitcherid is not present as a pitcher in this game")
  }

}

#' Helper function that splits the pitcher's name into "Last" "First"
#'
#' @param data pitcher dataset
#'
#' @return Two strings: "Last" "First"
split_pitcher_name <- function(data){

  # Split the pitcher's name from "Last, First" to "Last" "First"
  pitcher_name <- data$Pitcher[1]
  name_parts <- strsplit(pitcher_name, ", ")[[1]]
  return(name_parts)

}

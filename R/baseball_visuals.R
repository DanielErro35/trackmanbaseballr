#' Function to create plots from a pitcher's Trackman data
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#' @param type type of chart (movement, release, location)
#'
#' @return A scatter plot displaying desired pitch data specified by the chart type
#'
#' @import ggplot2
#' @import dplyr
#' @import png
#'
#' @export
pitcher_chart <- function(data, pitcherid, type = "movement"){

  # check that pitcherid is a number
  check_pitcherid(data, pitcherid)

  pitcher_data <- data %>%
    filter(PitcherId == pitcherid)

  # Retrieve pitcher name
  name_parts <- split_pitcher_name(pitcher_data)
  pitcher_lastname <- name_parts[1]
  pitcher_firstname <- name_parts[2]

  # Retrieve date from game
  game_date <- pitcher_data$Date[1]

  if(type == "movement"){
    movement_chart(pitcher_data, pitcher_firstname, pitcher_lastname, game_date)
  } else if(type == "release"){
    release_chart(pitcher_data, pitcher_firstname, pitcher_lastname, game_date)
  } else if(type == "location"){
    location_chart(pitcher_data, pitcher_firstname, pitcher_lastname, game_date)
  } else {
    stop("Please input a valid chart type (\"movement\", \"release\", \"location\")")
  }

}


#' Helper function that creates a movement plot
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A scatter plot displaying pitch movement profiles by pitch type
movement_chart <- function(data, firstname, lastname, game_date){

  data %>%
    ggplot(aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    ggtitle(glue::glue("{firstname} {lastname}: Movement Profile ({game_date})")) +
    xlab("Horizontal Break (in)") +
    ylab("Induced Vertical Break (in)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-30, 30) +
    ylim(-30, 30) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    theme_minimal()

}


#' Helper function that creates a release point plot
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A scatter plot displaying pitcher's release points by pitch type
release_chart <- function(data, firstname, lastname, game_date){

  data %>%
    ggplot(aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    geom_point(stat = "identity") +
    ggtitle(glue::glue("{firstname} {lastname}: Release Point ({game_date})")) +
    xlab("Release Side (ft)") +
    ylab("Release Height (ft)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-4, 4) +
    ylim(0, 8) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    theme_minimal()

}


#' Helper function that creates a pitch location plot
#'
#' @param data trackman baseball dataset
#' @param pitcherid The ID of pitcher
#'
#' @return A scatter plot displaying pitch locations by pitch type
location_chart <- function(data, firstname, lastname, game_date){

  # import strike zone image
  strike_zone <- readPNG("strikezone.png")

  data %>%
    ggplot(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    annotation_raster(strike_zone, -0.75, 0.75, 0, 4.5) +
    geom_point(stat = "identity", size = 3) +
    ggtitle(glue::glue("{firstname} {lastname}: Pitch Location ({game_date})")) +
    xlab("Horizontal Location (ft)") +
    ylab("Vertical Location (ft)") +
    guides(color = guide_legend(title = "Pitch Type")) +
    xlim(-3, 3) +
    ylim(0, 5)  +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

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

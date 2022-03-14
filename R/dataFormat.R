#' Function to format the .csv data generated from camtrapR

#' @author "SMandujanoR"
#' @param data_Spp Matrix of species
#' @param data_CT Matrix of camera-traps
#' @param speciesCol Spp
#' @param stationCol CT
#' @param setupCol Initial
#' @param retrievalCol End
#' @param CTDateFormat Date
#' @importFrom utils head
#' @importFrom camtrapR surveyReport
#' @importFrom fuzzySim spCodes
#' @importFrom dplyr %>% rename
#' @export

utils::globalVariables(c("Station", "n_events", "n_nights_active"))


dataFormat <- function(data_Spp, data_CT, speciesCol, stationCol, setupCol, retrievalCol, CTDateFormat) {

  #-----------------------------
  # Use the function surveyReport to modified the parameters:

  report <- surveyReport (recordTable = data_Spp, CTtable = data_CT, speciesCol = speciesCol, stationCol = stationCol, setupCol = setupCol, retrievalCol = retrievalCol, CTDateFormat = CTDateFormat, recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", CTHasProblems = T)

  sampling_effort <- report[[1]]

  days <- sampling_effort[c("Station", "n_nights_active")]

  species <- report[[5]]
  head(species)

  #-----------------------------
  # Merge data.frame:
  wildlife.data <- merge(species, days, all.y = T)

  #-----------------------------
  # Rename columns:

  wildlife.data <- wildlife.data %>%
    rename(Camera= Station, Events= n_events, Effort= n_nights_active)

  #-----------------------------
  # Species names abbreviation:

  wildlife.data$Species <- spCodes(wildlife.data$Species, sep.spcode = "_")

  #-----------------------------
  # Save the formated data:
  write.csv(wildlife.data, "data/wildlife_data.csv")
}

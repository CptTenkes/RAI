#' Function to graph the RAI of selected species
#'
#' @author SMandujanoR
#' @param mapa Introduce map
#' @param species Select the species
#' @param pointSize X
#' @param mycolors Select
#' @importFrom prettymapr addscalebar addnortharrow
#' @importFrom utils read.csv
#' @export


RAImapSp <- function(mapa, species, pointSize, mycolors) {

  ###setClass("RAImapSp", slots = list(mapa = "SpatialPolygonsDataFrame", species = "character", pointSize = "numeric", mycolors = "character"))

  new.mat <- read.csv("Results/Table_RAIalt.csv", header = T)
  sp  <- subset(new.mat, Species == species)

  jpeg(filename= paste("Results/Map_", str_to_title(species), ".jpg", sep = ""), width= 7000, height= 5000, units= "px", res=1200)

  par(mfrow = c(1,1), mar = c(1,1,1,1))

  plot(mapa, col = mycolors, fill = T, lty = 0, main = unique(sp$Species))
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()

  points(sp$X, sp$Y, pch = 16, col = "#00000170", cex = sp$RAIalt*pointSize)
  text(sp$X, sp$Y, sp$RAIalt, cex = 0.3)

  dev.off()
}

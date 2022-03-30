#' Function to kernel utilization distribution for selected species
#'
#' @author SMandujanoR
#' @param species x
#' @param S s
#' @param HR s
#' @param pointSize s
#' @param proje s
#' @param mycolors x
#' @importFrom prettymapr addscalebar addnortharrow
#' @importFrom utils read.csv
#' @importFrom graphics legend lines image
#' @importFrom grDevices rgb
#' @importFrom methods as
#' @importFrom adehabitatHR kernelUD getverticeshr
#' @importFrom sp spTransform SpatialPoints bbox coordinates<-
#' @importFrom raster extent extent<- writeRaster rasterize raster projection<-
#' @export


RAIkernelSp <- function(species, S, HR, pointSize, proje, mycolors) {

  # ----------------------------
  # Read RAIalt table:

  new.mat <- read.csv("Results/Table_RAIalt.csv", header = T)
  new.mat  <- subset(new.mat, Species == species)
  sp <- select(new.mat, c("X", "Y", "Events"))

  # ----------------------
  # Preparation of data:

  n <- length(sp$Events)
  nueva_mat <- vector("numeric", n)

  repetir <- function(i) {
    do.call("rbind", replicate(sp$Events[i], sp[i,], simplify = F))
  }

  for (i in 1:n) {
    nuevo <- repetir(i)
    nueva_mat <- rbind(nueva_mat, nuevo)
  }

  nueva_mat
  (species_coord <- nueva_mat[-1,-3])

  presenciaSP <- SpatialPoints(coords = species_coord)
  projection(presenciaSP) <- proje

  # ----------------------
  # Kernel analysis using adehabitatHR package

  S <- S # Extent
  HR <- HR # Home range

  (speciesUD <- kernelUD(presenciaSP, h = "href", kern = "bivnorm", grid = 95, hlim = c(0.75, 0.75), extent = S))
  (speciesHR <- getverticeshr(speciesUD, percent = HR))

  jpeg(filename= paste("Results/Kernel_", str_to_title(species), ".jpg", sep = ""), width= 8000, height= 7000, units= "px", res=1200)

  par(mfrow = c(1,1), mar = c(1,1,1,1))

  mi_mapaL <- as(mi_mapa, "SpatialLinesDataFrame")
  mi_mapaL <- spTransform(mi_mapaL, proje)

  image(speciesUD, col = mycolors, interpolate = F, cex.main = 1, main = paste("\n \n Species =", unique(new.mat$Species), "\n Utilization distribution of ", HR, "% = ", round(speciesHR$area, 1), "ha"), cex.main = 1.2)

  plot(mi_mapaL, add = T, col = "gray")
  points(sp$X, sp$Y, pch = 16, col = rgb(1, 0, 0, 0.75), cex = sp$RAIalt/pointSize)
  plot(speciesHR, add = T)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()

  dev.off()

  # ------------------
  # Create a Raster layer:

  Kernel_capa <- cbind(speciesUD@coords, speciesUD@data)
  kernel_data <- as.data.frame(Kernel_capa)
  coordinates(kernel_data) <- ~Var2 + Var1
  r <- raster(ncol = 60, nrow = 30)
  extent(r) <- extent(mi_mapaL)
  vals <- kernel_data$ud
  kernel_rast <- rasterize(kernel_data, r, vals)

  jpeg(filename = paste("Results/KernelRaster_", str_to_title(species), ".jpg", sep = ""), width= 8000, height= 7000, units= "px", res=1200)

  plot(kernel_rast)

  dev.off()

  writeRaster(kernel_rast, paste("Results/KernelRaster_", str_to_title(species), ".tiff", sep = ""), overwrite = T)
}

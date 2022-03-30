#' Function to create a camera-trap grid
#'
#' @author SMandujanoR
#' @param n1 Number of camera-traps in columns
#' @param n2 Number of camera-traps in rows
#' @param CTx Define coordinates X in UTMs
#' @param CTy Define coordinates Y in UTMs
#' @param CTdist Define distance among camera-traps
#' @importFrom prettymapr addscalebar addnortharrow
#' @importFrom utils read.csv
#' @importFrom graphics legend lines
#' @export


mi_Grid <- function(n1, n2, CTx, CTy, CTdist) {

  # -------------------
  # To create grid:

  jpeg(filename = "Results/CT_grid.jpg", width = 8000, height = 7000, units = "px", res = 1200)

  par(mfrow = c(1,1), mar = c(1,1,1,1))

  plot(mi_mapa, col = mi_paleta, fill = T, lty = 0)
  addscalebar(htin = 0.05, padin = c(0.05, 0.05), pos = "bottomright")
  addnortharrow()

  # -------------------
  # To create the grid:

  x <- seq(from = CTx, to = (CTx + n1*CTdist-CTdist), by = CTdist)
  y <- seq(from = CTy, to = (CTy + n2*CTdist-CTdist), by = CTdist)
  xy <- expand.grid(x = x, y = y)
  points(xy, col = "black", pch = 16, cex = 1.5)
  n <- as.character(1:(n1*n2))
  text(xy, n, col = "white", cex = 0.5)
  cat("-------- \n UTMs \n")
  print(xy)
  write.csv(xy, "Results/UTMs_grid.csv")

  # -------------------
  # To create buffer around the grid:

  xySP <- SpatialPoints(coords = xy)
  projection(xySP) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  class(xySP)
  bbox(xySP)
  xSP <- seq(from = bbox(xySP)[1]-CTdist, to = bbox(xySP)[3]+CTdist, by = n1*CTdist+CTdist)
  ySP <- seq(from = bbox(xySP)[2]-CTdist, to = bbox(xySP)[4]+CTdist, by = n2*CTdist+CTdist)
  xy2 <- expand.grid(x = xSP, y = ySP)
  points(xy2, col = "black", bg = "red", pch = 22, cex = 1)
  buf <- rbind(xy2[1,], xy2[2,], xy2[4,], xy2[3,], xy2[1,])
  lines(buf, add = T, col = "red", lty = 1, lwd = 3)

  # -------------------
  # To estimate area:

  cat("---------- \n Grid hectares = \n ")
  print(S_grid1 <- (((n1-1)*CTdist) * ((n2-1)*CTdist))/10000)

  cat("------------- \n Grid + buffer = \n ")
  print(S_grid2 <- (((n1+1)*CTdist) * ((n2+1)*CTdist))/10000)

  legend("bottomleft", c(paste("Grid of ", (n1*n2), "camera-traps,", "at", CTdist, "meters,"), paste("Grid area = ", S_grid1, "hectares,"), paste("and grid + buffer =", S_grid2, "hectares (red quadrant).")), cex = 0.8)

  dev.off()
}

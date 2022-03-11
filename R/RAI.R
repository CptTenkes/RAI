#' Function to calculate the classic or general RAI model

#' @author "SMandujanoR"
#' @param new.mat Matrix with data of camera-traps
#' @param Species Select the species to plot
#' @param parRow Number of rows
#' @param parCol Number of columns
#' @param pointSize Select the size of the circles
#' @param RAIgral General RAI
#' @importFrom grDevices dev.off jpeg
#' @importFrom graphics abline plot text par points
#' @importFrom stats cor lm
#' @importFrom utils write.csv
#' @export

RAIgral <- function(new.mat, Species, parRow, parCol, pointSize) {

  # ---------------------------------
  ## RAIgral estimation:

  Tot_cameras <- with(new.mat, length(unique(Camera)))
  cameras <- with(new.mat, tapply(Camera, Species, length))
  days <- with(new.mat, tapply(Effort, Species, sum))
  n <- with(new.mat, tapply(Events, Species, sum))
  RAIgral <- round(n/days*100, 2)

  # ---------------------------------
  ## Naive occupation estimation:

  occ <- subset(new.mat, new.mat$Events > 0)
  OccNaive <- as.data.frame(round(table(occ$Species)/cameras,2))

  # ---------------------------------
  ## Results:

  table1 <- cbind(cameras, days, n, RAIgral, OccNaive = OccNaive[,2])
  table1 <- table1[order(RAIgral),]
  print(table1)
  write.csv(table1, "Results/Table_RAIgral.csv")

  # ---------------------------------
  ## RAI-naive occupation graph:

  jpeg(filename = "Results/Dist_Abun.jpg", width = 7000, height = 5000, units = "px", res = 1200)

  r2 <- round(cor(RAIgral, OccNaive[,2]),2)
  plot(OccNaive[,2], RAIgral, xlab= "Distribution (Naive_Occ)", ylab = "Relative abundance (RAI)", main = paste("r2 =", r2), frame.plot = F, las = 1, pch = 16, col = "skyblue", cex = 2)
  abline(lm(RAIgral ~ OccNaive[,2]), col = "red", lwd = 2)
  text(OccNaive[,2], RAIgral, OccNaive[,1], cex = 0.5)

  dev.off()

  # ---------------------------------
  ## Events distribution graph:

  jpeg(filename = "Results/Dist_Spp.jpg", width = 7000, height = 5000, units = "px", res = 1200)

  par(mfcol = c(parRow, parCol), mar = c(3,1,1,1))

  for (i in 1:length(Species)) {
    sp  <- subset(new.mat, Species == Species[i])

    plot(sp$X, sp$Y, xlab = "", ylab = "", frame.plot = F, cex.axis = 1, main = unique(sp$Species), type = "n", labels = F, cex.main = 0.9)
    points(sp$X, sp$Y, pch = 16, col = "skyblue", cex = sp$Events*pointSize)
    text(sp$X, sp$Y, sp$Events, cex = 0.7)
  }
  dev.off()
}

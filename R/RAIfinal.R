#' Function to create final Table
#'
#' @author SMandujanoR
#' @param new.mat Matrix with data of camera-traps
#' @importFrom utils read.csv
#' @export

RAIfinal <- function(new.mat) {
  t1 <- read.csv("Results/Table_RAIgral.csv", header = T)
  t2 <- read.csv("Results/Table_RAIalt.csv", header = T)
  t3 <- read.csv("Results/Table_Spp_GLM.csv", header = T)

  RAIalt.mean <-  with(t2, round(tapply(RAIalt, Species, mean), 2))
  RAIalt.sd <- with(t2, round(tapply(RAIalt, Species, sd), 2))
  alt <- cbind(RAIalt.mean, RAIalt.sd)
  alt2 <- alt[order(RAIalt.mean),]

  table3 <- with(c(t1,t3), cbind(cameras, days, n, RAIgral, alt2, RAIpoisson, OccNaive))
  #print(table3)
  write.csv(table3, "Results/Table_Final.csv")
}

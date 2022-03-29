#' Function GLM Poisson model with "offsets"
#'
#' @author SMandujanoR
#' @param new.mat Matrix with data of camera-traps
#' @param family Choise among "Gaussian", "Poisson", "quasipoisson" distribution
#' @importFrom xtable xtable
#' @importFrom stats glm
#' @importFrom utils read.csv
#' @export


RAIglm <- function(new.mat, family = family) {
  RAIglm <- glm(Events ~ Species-1, data = new.mat, offset = log(Effort), family = family)
  cat("----- \n GLM-Poisson: \n")
  print(summary(RAIglm))
  modglm <- summary(RAIglm)

  RAIpoisson <- cbind(RAIpoisson = round(exp(RAIglm$coefficients)*100, 2))
  RAIpoisson <- RAIpoisson[order(RAIpoisson),]
  table_glm <- xtable(modglm)
  mlg <- cbind(table_glm, RAIpoisson)
  write.csv(mlg, "Results/Table_Spp_GLM.csv")
}

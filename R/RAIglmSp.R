#' Function to GLM-Poisson for selected species and analysis of one covariate
#'
#' @author SMandujanoR
#' @param Species Select the species
#' @param covs Define covariates
#' @param family Define the error distribution
#' @importFrom utils read.csv
#' @importFrom stringr str_to_title
#' @importFrom xtable xtable
#' @importFrom stats glm
#' @importFrom dplyr %>% select
#' @export

RAIglmSp <- function(Species, covs, family) {

  ## Select species:

  new.mat <- read.csv("Results/Table_RAIalt.csv", header = T)
  Sp <- new.mat[new.mat$Species== Species,]
  Sp <- Sp %>% select(covs, Species, Events, Effort, RAIalt)
  print(Sp)
  write.csv(Sp, paste("Results/Table_", str_to_title(Species), ".csv", sep = ""))

  # ---------------------------------
  ## GLM model for each covariate

  for (i in 1:length(covs)) {

    RAIglm1 <- glm(Events ~ Sp[,i]-1, offset = log(Effort), family, data = Sp)

    cat("---------------------------- \n RAI GLM-Poisson with covariates: \n")
    print(covs[i])
    print(summary(RAIglm1))
    table_glm <- xtable(RAIglm1)
    Cov <- covs[i]
    write.csv(table_glm, paste("Results/Table_GLM_Sp_", str_to_title(Cov), ".csv", sep = ""))

    jpeg(filename= paste("Results/GLM_Sp_", str_to_title(Cov), ".jpg", sep = ""), width= 7000, height= 5000, units= "px", res=1200)

    par(mfrow = c(1,1), mar = c(1,1,1,1))

    plot(RAIalt ~ Sp[,i], data = Sp, frame.plot = F, col = "skyblue", pch = 16, xlab = names(Sp[1]), main = Cov)

    dev.off()
  }
}

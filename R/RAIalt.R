#' Function to calculate the alternative RAI model
#'
#' @author "SMandujanoR"
#' @param new.mat Matrix with data of camera-traps
#' @param max Select maxime value to plot Y-axes
#' @importFrom graphics boxplot
#' @importFrom stats aov TukeyHSD summary.aov
#' @importFrom xtable xtable
#' @importFrom agricolae HSD.test
#' @importFrom dplyr select arrange
#' @importFrom tidyr pivot_wider
#' @export


RAIalt <- function(new.mat, max) {
  RAIalt <- with(new.mat, round((Events/Effort)*100, 2))
  table2 <- cbind(new.mat, RAIalt)
  write.csv(table2, "Results/Table_RAIalt.csv")

  jpeg(filename = "Results/RAIalt.jpg", width = 7000, height = 5000, units = "px", res = 1200)

  par(mfcol = c(1,1), mar = c(5,5,5,5))
  boxplot(RAIalt ~ Species, data = new.mat, ylab = "RAI", ylim = c(0, max), xlab = "", varwidth = F, outline = F, cex = 3.4, las = 2, frame.plot = F, cex.axis = 0.7, col = "skyblue")
  dev.off()

  # ---------------------------------
  # Statistical comparison

  RAIaov <- aov(RAIalt ~ Species-1, data = table2)
  cat("----- \n RAI comparasion among species \n")
  mod <- summary(RAIaov)
  anova <- xtable(mod)
  write.csv(anova, "Results/Table_ANOVA.csv")

  # ---------------------------------
  # Posterior comparison

  P <- as.numeric(unlist(summary.aov(RAIaov)[[1]][5]))[1]

  ifelse(P < 0.05, {
    # if P significative then:

    ## Tukey test

    jpeg(filename = "Results/Tukey_test.jpg", width = 7000, height = 10000, units = "px", res = 1200)

    par(mfcol = c(1,1))
    plot(TukeyHSD(RAIaov), cex.axis = 0.5, las = 1)
    dev.off()

    # HSD test

    #require(agricolae)
    out_1 <- HSD.test(RAIaov, "Species")
    out2 <- out_1$groups

    jpeg(filename = "Results/HSD_test.jpg", width = 7000, height = 5000, units = "px", res = 1200)

    par(mfcol = c(1,1))
    bar.group(out_1$groups, horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, max),   names.arg = out_1$trt, ylab = "RAI")
    dev.off()

  }, NA) # if P not significant

  # ---------------------------------
  # Create array of species by RAI and Events:

  table2 <- read.csv("Results/Table_RAIalt.csv", header = T)
  SppRAI <- table2 %>% select(Camera, Species, RAIalt)
  SppRAI <- arrange(SppRAI, Species)
  SppRAI <- pivot_wider(SppRAI, names_from = "Species", values_from =  "RAIalt")
  write.csv(SppRAI, "Results/Table_Spp_IAR.csv")

  SppEvents <- table2 %>% select(Camera, Species, Events)
  SppEvents <- arrange(SppEvents, Species)
  matSpp_Event <- pivot_wider(SppEvents, names_from = "Species", values_from =  "Events")
  write.csv(matSpp_Event, "Results/Table_Spp_Events.csv")

}

Load_FVR_Dependendencies <- function(update_FVR = FALSE) {
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)

  if (!require(Rcpp)) install.packages("Rcpp")
  library(Rcpp)

  if (!require(dplyr)) install.packages("dplyr")
  library(dplyr)

  if (!require(reticulate)) install.packages("reticulate")
  library(reticulate)

  if (!require(devtools)) install.packages("devtools")
  library(devtools)

  if (!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)

  if (update_FVR == TRUE) {
    install_github("ftkjr/FredsVietorisRips")
    library(FredsVietorisRips)
  }
}

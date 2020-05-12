Plot_0_Simplices <- function(x, y){
  ###### Packages ####
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)
  if (!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)

  ##### Temporary Data Frame ####
  df <- data.frame(
    x = x,
    y = y,
    Point = paste0("P", c(1:length(x)))
  )

  ##### Create Graph ####
  graph <- df %>%
    ggplot() +
    geom_point(data = df,
               aes(x, y))

  ##### Return the Graph ####
  return(graph)
}

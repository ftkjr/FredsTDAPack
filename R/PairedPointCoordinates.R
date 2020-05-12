PairedPointCoordinates <- function(paired_points, df){
  ##### Packages ####
  if (!require(reshape2)) install.packages("reshape2")
  library(reshape2)
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)
  if (!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)

  ##### Combine the Paired Points with their Coordinates ####
  ppc <- paired_points %>%
    melt(measure.vars = c("Point_1", "Point_2")) %>%
    select(group, value) %>%
    rename(Point = value) %>%
    left_join(df, by = "Point")

  ##### Return the new frame ####
  return(ppc)
}

PlotEpsilonFrame <- function(epsilon_frame, n_points) {
  ##### Import Plotting Library ####
  library(ggplot2)

  ##### Create Chart Framework ####
  chart <- epsilon_frame %>%
    ggplot(aes(x = Epsilon)) +
    geom_line(aes(y = Connections), color = "red") +
    geom_line(aes(y = Components / n_points * choose(n_points, 2)), color = "blue") +
    ylab("Connections (Red)") +
    scale_y_continuous(sec.axis = sec_axis(~.* n_points / choose(n_points, 2), name = "Components (Blue)"))

  return(chart)
}

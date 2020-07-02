TidyDistanceFrame <- function(mat) {

  ##### Begin with Tidy Combination Pairs ####
  df <- t(combn(ncol(mat), 2))

  df <- cbind(df, rep(0, ncol(df)))

  ##### Populate col 3 with Value from Matrix Location ####
  for (r in c(1:choose(nrow(mat), 2))) {
    df[r, 3] <- mat[ df[r, 1] , df[r, 2] ]
  }

  ##### Set Names ####
  colnames(df) <- c("Element1", "Element2", "Distance")

  ##### as.data.frame ####
  df <- as.data.frame(df)

  ##### Return Frame ####
  return(df)


}

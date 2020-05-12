Two_Simplex <- function(x, y, epsilon, triple_matrix) {
  for (i in nrow(triple_matrix)) {
    if (centroid(x[triple_matrix[i, ]], y[triple_matrix[i, ]] >= epsilon/2)){
      triple_matrix <- triple_matrix[-i, ]
    }
  }
  return(triple_matrix)
}

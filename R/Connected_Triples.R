Connected_Triples <- function(adjacency_triangle){
  n <- ncol(adjacency_triangle)
  triple <- vector()

  triple_matrix <- matrix(
    ncol = 3
    )
  for (i in c(1:n)) {
    for(j in c(1:n)) {
      if (i < j) {
        if (adjacency_triangle[i, j] == 1) {

          for (k in c(i:n)) {
            if (j < k) {
              if (adjacency_triangle[i, k] == 1){
                triple <- cbind(i, j, k)
                triple_matrix <- rbind(triple_matrix, triple)
                } #endif
              }#endif
            }#endfor
          }#endif
        }#endif
      }#endfor
    }#endfor

  ##### Remove rows where all entries are NA ####
  triple_matrix <-
    triple_matrix[rowSums(is.na(triple_matrix)) != ncol(triple_matrix), ]

  ##### Attach Dimension Names ####
  dimnames(triple_matrix) <- list(
    paste0("T", c(1:nrow(triple_matrix))),   # Row Names
    paste0("P", c(1:3))                      # Column Names
  )

  ##### Return ####
  return(triple_matrix)
  }

#' @title JaccardDissimilarityMatrix
#'
#' @param text_vector a vector of sets of text
#'
#' @return an n X n matrix of the Jaccard Dissimilarity between points
#'
JaccardDissimilarityMatrix <- function(text_vector) {

  ##### External Packages ####
  library(text2vec)
  library(magrittr)

  ##### Tokenize Sample ####
  it_sample <- text_vector %>%
    itoken(progressbar = FALSE)

  ##### Vectorize (???) ####
  vectorizer <- text_vector %>%
    itoken(progressbar = FALSE) %>%
    create_vocabulary() %>%
    prune_vocabulary() %>%
    vocab_vectorizer()

  ##### Document Term Matrix ####
  dtm_sample <- it_sample %>%
    create_dtm(vectorizer)

  ##### Jaccard Similarity ####
  jacsim <- sim2(dtm_sample, dtm_sample, method = "jaccard", norm = "none")

  ##### Convert fancy object to matrix ####
  jsmat <- jacsim %>%
    as.matrix()

  ##### Jaccard Distance ####
  # Flip it around so 1 is max distance
  # and 0 indicates two sets are the same
  jacdismat <- 1 - jsmat

  return(jacdismat)
}

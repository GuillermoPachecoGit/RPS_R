#' This function performs the classical least-squares (LS)
#' Procrustes superimposition of the input configurations of landmarks
#'
#' @param X A s-dimensional array (s=2 or s=3) of n x k matrices, representing shapes of k objects through n landmarks in s dimensions
#'
#' @return s-dimensional array of n x k matrices, representing the different specimens after adjusting.
#'
#' @author Guillermo Pacheco, Viviana Ferraggine, Sebastian Torcida
#' @export
procrustesCM_RPS <- function(X) {
  result <- geomorph::gpagen(X)
  return(result['coords'][[1]])
}

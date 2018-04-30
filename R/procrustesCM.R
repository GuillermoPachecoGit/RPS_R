#' This function performs the adjusting of the different specimens by least squares
#'
#' @param X A s-dimensional array of n * k matrix, representing the different specimens to be analyzed
#'
#' @return s-dimensional array of n * k matrix, representing the different specimens after adjusting.
#'
#' @author Guillermo Andres Pacheco, Viviana Elizabeth Ferraggine, Sebastian Torcida
#' @export
procrustesCM <- function(X) {
  result <- geomorph::gpagen(X)
  return(result['coords'][[1]])
}

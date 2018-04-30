#' This procedure deletes a landmark (row) from the input nx3 matrix
#'
#' @param x A nx3 matrix (n is the number of landmarks)
#' @param num_landmark The number of landmark (row) to be deleted
#' @return x The corresponding (n-1)x3 output matrix
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine, Sebastian Torcida
#' @export
deletePoint <- function(x, num_landmark) {
    x <- x[-num_landmark, ]
    return(x)
}

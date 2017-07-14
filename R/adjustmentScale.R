#' This function scales each matrix from the input set
#' to make its median interlandmark distance equal 1
#'
#' @param X A set of n x k matrices, each representing a centered object
#' @param r Number of matrices (objects) in the set
#' @return The corresponding set of scaled matrices
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
adjustmentScale <- function(X, r) {
    for (k in 1:r) {
        s <- medland(X[, , k], 2)
        X[, , k] <- X[, , k]/s
    }
    return(X)
}

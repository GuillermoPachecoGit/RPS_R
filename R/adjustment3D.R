#' This procedure converts an input set of n x 2 matrices (representing 2D objects)
#' into a set of the corresponding n x 3 matrices, by adding each a column of 0s
#' (here n is the number of matrix rows or landmarks)
#' 
#'
#' @param X  The input set of n x 2 matrices, representing objects in 2D
#' @return The output set of n x 3 matrices
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
adjustment3D <- function(X) {
    f <- nrow(X[, , 1])  # number of landmarks in every object
    r <- ncol(X[, 1, ])  # number of objects in the set

    R <- array(matrix(nrow = f, ncol = 3, 0), c(f, 3, r), dimnames = NULL)

    # a last column of 0s is added to each object
    for (i in 1:r) {
        R[, , i] <- cbind(X[, , i], 0)
    }

    return(R)

}

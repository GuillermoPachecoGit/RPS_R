#' This function returns a nxn square matrix P with the initial estimates
#' of the scaling factor associated to matrix X, to achieve optimal matching
#' in scale with consensus matrix Y
#'
#' @param X The input nxk matrix to be scaled
#' @param Y The reference pivotal or consensus matrix
#'
#' @return A square matrix P with initial estimates of the scaling factor for X
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
escala <- function(X, Y) {
    n <- nrow(X)
    P <- matrix(nrow = n, ncol = n, 1)

    for (i in 1:n) {
        for (j in i + 1:n) {
            if (j <= n) {
                P[i, j] <- norm(t(as.matrix(Y[i, ])) - t(as.matrix(Y[j, ])), "F")/norm(t(as.matrix(X[i,
                  ])) - t(as.matrix(X[j, ])), "F")
                P[j, i] <- P[i, j]
            }
        }

    }

    return(P)
}


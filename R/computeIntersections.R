#' Description
#'
#' @param
#' @param
#' @return s
#'
#' @aliases
#' @family
#' @export
#'
#' @examples
#'
#' @author Guillermo Andres Pacheco
computeIntersections <- function(X, ii, D) {
    k <- nrow(X)
    nl <- ncol(X)
    V <- X - matlab::repmat(t(t(as.matrix(X[, ii]))), 1, nl)
    Daux <- t(t(sqrt((apply(V * V, 2, sum)))))

    Q <- as.matrix(D[ii, ])/Daux  #mirar el operador

    Z <- X - V * matlab::repmat(Q, k, 1)
    Z <- Z[, -ii]
    return(Z)
}

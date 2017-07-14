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
spmedcrudo <- function(X) {
    tol <- 1e-07
    iter <- 1000
    n <- nrow(X)
    p <- ncol(X)

    A <- matrix(nrow = n, ncol = p, 0)
    A <- X

    w <- matrix(nrow = 1, ncol = n, 1)
    s <- matrix(nrow = 1, ncol = n, 0)

    aux <- matrix(nrow = 1, ncol = p, 0)
    aux <- apply(A, 1, median)
    auxant <- matrix(nrow = 1, ncol = p, 0)

    h <- 1

    while ((abs(aux - auxant) > tol) && (h <= iter)) {
        for (i in 1:n) {
            s[i] <- x[i]/norm(aux - t(as.matrix(A[i, ])))
        }
        auxant <- aux
        aux <- s * A/sum(s)
        h <- h + 1
    }
    return(aux)
}

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
distAllPairsL2 <- function(X) {
    q <- t(X) %*% X
    n <- ncol(q)
    normx <- matlab::repmat(as.matrix(apply(t(X)^2, 1, sum)), 1, n)
    K <- Re(sqrtm(q * (-2) + normx + t(normx)))
    K <- K - (diag(diag(K)))
    return(K)
}

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
translation <- function(X, Y, f) {
    TR <- (Y - X)
    t <- matrix(nrow = 1, ncol = 3, 0)
    t <- apply(TR, 2, median)
    X <- X + (matrix(nrow = f, ncol = 1, 1) %*% t(as.matrix(t)))

    return(X)

}

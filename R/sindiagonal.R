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
sindiagonal <- function(B) {

    f <- nrow(B)
    c <- (ncol(B) - 1)
    A <- matrix(nrow = f, ncol = c, 0)

    for (i in 1:f) {
        for (j in 1:c) {
            if (i <= j) {
                A[i, j] <- B[i, j + 1]
            } else {
                A[i, j] <- B[i, j]
            }
        }
    }
    return(A)
}

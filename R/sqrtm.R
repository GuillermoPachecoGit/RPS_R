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
sqrtm <- function(x) {
    z <- x
    for (i in 1:ncol(x)) {
        for (j in 1:nrow(x)) {
            z[i, j] <- sqrt(as.complex(x[i, j]))
        }
    }
    return(z)
}

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
CrossProduct3D <- function(x, y, i = 1:3) {
    # Project inputs into 3D, since the cross product only makes sense in 3D.
    To3D <- function(x) head(c(x, rep(0, 3)), 3)
    x <- To3D(x)
    y <- To3D(y)

    # Indices should be treated cyclically (i.e., index 4 is 'really' index 1, and so on).
    # Index3D() lets us do that using R's convention of 1-based (rather than 0-based) arrays.
    Index3D <- function(i) (i - 1)%%3 + 1

    # The i'th component of the cross product is: (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
    # as long as we treat the indices cyclically.
    return(x[Index3D(i + 1)] * y[Index3D(i + 2)] - x[Index3D(i + 2)] * y[Index3D(i + 1)])
}

vector.cross <- function(a, b) {

    if (length(a) != 3 || length(b) != 3) {
        a = c(a, 0)
        b = c(b, 0)
    }
    i1 <- c(2, 3, 1)
    i2 <- c(3, 1, 2)
    return(a[i1] * b[i2] - a[i2] * b[i1])

}

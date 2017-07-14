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
#' @exports
univMDSrobust <- function(D, k) {
    iteraciones <- 10
    tol <- 1e-09
    nl <- nrow(D)  #numer of specimens
    X <- t(randomMatrix(nl, k))
    Dk <- distAllPairsL2(X)
    c <- sum((D - Dk))
    cant <- 0

    for (iter in 1:iteraciones) {
        #print(iter)
        for (ii in 1:nl) {
            for (it in 1:floor(sqrt(iter))) {
                Z <- computeIntersections(X, ii, D)
                b <- Robgit2012::spatialmed_landmark(t(Z))  #porque Z transpuesto??
                a <- t(b)

                x1 <- cbind(t(t(X[, 1:ii - 1])), a)
                if ((ii + 1) <= nl) {

                  X <- cbind(x1, t(t(X[, (ii + 1):nl])))

                } else {
                  X <- x1
                }



            }
        }

        Dk <- distAllPairsL2(X)
        cant <- c
        c <- sum(sum((D - Dk)))
        if (abs(c - cant) < tol) {
            break
        }
    }

    return(t(X))
}

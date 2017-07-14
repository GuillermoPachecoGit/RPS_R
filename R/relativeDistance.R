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
relativeDistance <- function(X, specm1, specm2, t) {

    n <- NROW(X[, , 1])
    P <- matrix(nrow = n, ncol = 1, 0)

    if (t == 1) {
        dist <- distanciaCM(X)
        distTotal <- (dist[specm1, specm2])^2
    } else {
        dist <- distanciaR(X)
        distTotal <- dist[specm1, specm2]
    }

    for (land in 1:n) {
        # print(norm(t(as.matrix(X[land,,i]))-t(as.matrix(X[land,,j])))^2)
        if (t == 1) {
            distanciaLocal = norm(t(as.matrix(X[land, , specm1])) - t(as.matrix(X[land, , specm2])))^2
        } else {
            distanciaLocal = norm(t(as.matrix(X[land, , specm1])) - t(as.matrix(X[land, , specm2])))
        }


        P[land, 1] <- 100 * distanciaLocal/distTotal
    }

    return(P)
}

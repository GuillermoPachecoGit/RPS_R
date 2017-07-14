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
spatialmed_config <- function(X) {


    # inicializacion de variables
    n = nrow(X[, , 1])  #cantidad de landmarks
    p = ncol(X[, , 1])  #cantidad de dimensiones
    r = ncol(X[, 1, ])  #cantidad de ejeplares

    A <- matrix(nrow = r, ncol = p, 0)
    M <- matrix(nrow = n, ncol = p, 0)

    w <- matrix(nrow = 1, ncol = r, 1)
    s <- matrix(nrow = 1, ncol = r, 1)

    aux <- matrix(nrow = 1, ncol = p, 0)




    i <- 1
    for (i in 1:n) {
        for (k in 1:r) {
            A[k, ] = X[i, , k]
        }


        aux <- spatialmed_landmark(A)

        M[i, ] <- aux

        aux <- matrix(nrow = 1, ncol = p, 0)


        s <- matrix(nrow = 1, ncol = r, 0)
        A <- matrix(nrow = r, ncol = p, 0)
    }




    return(M)
}


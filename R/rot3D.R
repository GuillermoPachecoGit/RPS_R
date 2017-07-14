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
rot3D <- function(X, Y) {
    eje <- matrix(nrow = 1, ncol = 3, 0)
    tita <- 0

    f = nrow(X)  #cantidad de landmarks
    c = ncol(X)  # dim del espacio

    R <- matrix(nrow = c, ncol = c, 0)

    E <- array(matrix(nrow = f - 1, ncol = c, 0), c(f - 1, c, f), dimnames = NULL)


    ejes <- matrix(nrow = f, ncol = c, 0)
    efinal <- matrix(nrow = 1, ncol = c, 0)
    ejaux <- matrix(nrow = 1, ncol = 3, 0)
    Teta <- matrix(nrow = f, ncol = f, 0)
    for (i in 1:f) {
        for (j in 1:f) {
            if (i < j) {
                R <- estimorot3D(X, Y, i, j)
                if (i == 5) {
                  R
                }
                E[j - 1, , i] <- ejerot(R)
                Teta[i, j] <- angurot(R)
            } else if (i > j) {
                R <- estimorot3D(X, Y, i, j)
                E[j, , i] <- ejerot(R)
                Teta[i, j] <- angurot(R)
            }
        }
    }

    v <- matrix(nrow = 1, ncol = 3, 0)
    h <- 1
    for (i in 1:f) {
        v <- apply(E[, , i], 2, median)
        while ((norm(t(as.matrix(v)), "F") < 0.001) & (h < f)) {
            if (E[h, 3, i] == 1) {
            } else if (E[h, 1, i] < 0) {
                E[h, , i] <- -E[h, , i]
            }
            h <- (h + 1)
            v <- (apply(E[, , i], 2, median))
        }
        h <- 1
        ejes[i, ] <- v/norm(t(as.matrix(v)), "F")
        v <- matrix(nrow = 1, ncol = 3, 0)
    }
    w <- matrix(nrow = 1, ncol = 3, 0)
    for (k in 1:3) {
        w[1, k] <- median(ejes[, k])
    }
    w <- w/norm(w, "F")
    eje <- w
    tita <- medianarep(Teta)
    return(list(eje, tita))
}


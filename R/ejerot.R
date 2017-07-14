#' This function returns the rotation axis associated to the input rotation matrix R
#'
#' @param R The input 3x3 rotation matrix (representing a 3D rotation)
#'
#' @return The corresponding rotation axis
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
ejerot <- function(R) {

    toler = 1e-16
    for (i in 1:3) {
        for (j in 1:3) {
            if (abs(R[i]) < toler) {
                R[i] <- 0
            }
        }

    }

    # tolerancia
    tol = 1e-11
    dif = abs(R - diag(3))
    count = 1

    i = 1
    while (i <= 3) {
        j = 1
        while (j <= 3) {
            if (dif[[i, j]] < tol) {
                count = count + 1
            }
            j = j + 1
        }
        i = i + 1
    }

    # VER LA COMPARACION CON EL MODULO DE LA RESTA DE AMBAS MATRICES, USANDO UNA TOLERANCIA
    if (count < 9) {

        H <- R - diag(3)
        V <- Null(t(H) %*% H)
    } else {
        V <- matrix(nrow = 1, ncol = 3, 0)
        V[1, 3] <- 1
    }

    for (i in 1:3) {
        if (abs(V[i]) < tol) {
            V[i] <- 0
        }
    }


    if (V[3] < 0) {
        V <- -V
    }

    v <- t(V)
    return(V)
}

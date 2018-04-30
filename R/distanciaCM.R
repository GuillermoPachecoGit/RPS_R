#' This function computes the least squares distance between
#' each pair of matrices from the input set
#'
#'
#' @param X The input set of nx3 matrices (objects)
#'
#'
#' @return This function computes the least squares distance between each pair of matrices from the input set
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine, Sebastian Torcida
#' @export
distanciaCM <- function(X) {
    numEjemplares <- ncol(X[1, , ])
    nLandmark <- nrow(X[, , 1])


    D <- matrix(nrow = numEjemplares, ncol = numEjemplares, 0)


    for (i in 1:numEjemplares) {
        j <- (i + 1)
        while (j <= numEjemplares) {
            distanciaTotal <- 0
            for (land in 1:nLandmark) {
                # print(norm(t(as.matrix(X[land,,i]))-t(as.matrix(X[land,,j])))^2)
                distanciaLocal = norm(t(as.matrix(X[land, , i])) - t(as.matrix(X[land, , j])))^2
                distanciaTotal = distanciaTotal + distanciaLocal
            }
            D[i, j] <- distanciaTotal^0.5
            D[j, i] <- distanciaTotal^0.5
            j <- j + 1
        }
    }
    return(D)
}

#' This function computes the a resistant distance between
#' each pair of matrices from the input set
#'
#' @param X The input set of nx3 matrices (objects)
#'
#' @return This function computes the sum of non-squared euclidean distances across landmarks for each pair of matrices from the input set
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine, Sebastian Torcida
#' @export
resdistance_RPS <- function(X) {
    numEjemplares <- ncol(X[1, , ])
    nLandmark <- nrow(X[, , 1])

    D <- matrix(nrow = numEjemplares, ncol = numEjemplares, 0)

    for (i in 1:numEjemplares) {
        j <- (i + 1)
        while (j <= numEjemplares) {
            distanciaTotal <- 0
            for (land in 1:nLandmark) {
                # print(norm(t(as.matrix(X[land,,i]))-t(as.matrix(X[land,,j]))))
                distanciaLocal = norm(t(as.matrix(X[land, , i])) - t(as.matrix(X[land, , j])))
                distanciaTotal = distanciaTotal + distanciaLocal
            }
            D[i, j] <- distanciaTotal
            D[j, i] <- distanciaTotal
            j <- j + 1
        }
    }
    return(D)

}

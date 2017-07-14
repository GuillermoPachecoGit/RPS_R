#' This function performs a resistant centering of the objects to be superimposed
#' The spatial median of each object is made to be the origin of coordinates
#'
#' @param X The input set of r matrices of size nx3
#' @param f Number of landmarks
#' @param r Number of matrices (objects)
#' @return A corresponding set of r CENTERED matrices of size nx3
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
initialFocus <- function(X, f, r) {
    for (k in 1:r) {
        taux <- matrix(nrow = 1, ncol = 3, 0)  #aux. var. to store the actual center
        # taux <- componentwise median? HABRIA QUE INCLUIR ESTA OPCION...
        taux <- spatialmed_landmark(X[, , k])
        print(taux)
        X[, , k] <- X[, , k] - (matrix(nrow = f, ncol = 1, 1) %*% taux)  # lleva la madiana espacial de la configuracion al origen, junto con todos sus puntos.
    }

    return(X)
}

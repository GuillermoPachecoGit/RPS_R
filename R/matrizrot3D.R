#' Given a pair (e,a) where  'e' will be the rotation axis and
#' 'a' the corresponding rotation angle, the 3D rotation matrix R associated
#' to those parameters is obtained in its canonical form
#' 
#' @param e The input rotation axis
#' @param a The input rotation angle
#' @return The associated 3D rotation matrix R (canonical form).
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
matrizrot3D <- function(e, a) {

    AF <- matrix(nrow = 3, ncol = 3, 0)  # to store eigenvectors (as rows)
    Bloque <- matrix(nrow = 3, ncol = 3, 0)  # to store eigenvalues
    R <- matrix(nrow = 3, ncol = 3, 0)

    Bloque[3, 3] <- 1  # the eigenvalue of the axis is set
    Bloque[1:2, 1:2] = matrizrot(a)  # the 2D rotation of angle 'a' in the plante orthogonal to 'e'
    V = Null(t(e))  
    AF[3, ] <- e  # the axis is placed on the 3rd row
    AF[1:2, ] <- t(V)  #preguntar

    R <- t(AF) %*% Bloque %*% AF

    return(R)
}

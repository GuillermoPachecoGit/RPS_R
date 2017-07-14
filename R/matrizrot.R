#' Given a rotation angle, this function returns a 2D rotation matrix, 
#' @param a The input rotation angle
#'
#' @return The returned rotation matrix H
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
matrizrot <- function(a) {

    H <- matrix(nrow = 2, ncol = 2, 0)
    H[1, 1] <- cos(a)
    H[1, 2] <- sin(a)
    H[2, 1] <- -sin(a)
    H[2, 2] <- cos(a)

    return(H)
}

#' This function outputs the rotation angle associated with the input rotation matrix R.
#'
#' @param R The input 3x3 rotation matrix (a rotation in 3D)
#'
#' @return The rotation angle associated to R
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
angurot <- function(R) {
    # The rotation angle associated to R is computed in the range [-pi,pi]
    t = (sum(diag(R)) - 1)/2

    v <- acos(pmin(pmax(t, -1), 1))  # diagonal sum = trace in Matlab

    if (v < (-pi)) {
        v <- (v + (2 * pi))
    } else if (v > pi) {
        v <- (v - (2 * pi))
    }
    return(v)
}

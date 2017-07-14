#' This function returns the repeated median estimate of the parameter (scale factor or angle)
#' whose initial estimates are given in the input matrix A
#'
#' @param A The input matrix with the initial estimates of the parameter (angle or scaling factor)
#'
#' @return The (final) resistant estimate of the parameter, obtained through repeated medians
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
medianarep <- function(A) {
    ASIND <- sindiagonal(A)  # aux. matrix 
    f <- nrow(A)
    aux <- matrix(nrow = f, ncol = 1, 0)

    for (i in 1:f) {
        aux[i, 1] <- median(ASIND[i, ])  # obtengo un vector fila
    }
    m <- median(aux)
    return(m)
}

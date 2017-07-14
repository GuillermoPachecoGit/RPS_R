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
rotation <- function(X, H, Y) {
    eje <- matrix(nrow = 1, ncol = 3, 0)
    tita <- 0

    aux_list <- rot3D(X, Y)
    eje <- aux_list[[1]]
    tita <- aux_list[[2]]

    H <- matrizrot3D(eje, tita)
    X <- X %*% H
    return(list(X, H))
}

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
scaleSpecimen <- function(X, Y) {
    E <- escala(X, Y)
    ro <- medianarep(E)  # obtengo el factor de escala robusto , para ajustar la configuracion k-esima con la consenso
    X <- ro * X
    return(X)
}

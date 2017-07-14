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
randomMatrix <- function(NRows, NCols) {
    myMat <- matrix(runif(NCols * NRows), ncol = NCols)
    return(myMat)
}

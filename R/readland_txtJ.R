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
#' @exports
readland.txtJ <- function(path, dim, sim) {
    result <- read.table(path)  #get data from the file into an array

    s <- nrow(result)  #number of specimens
    p <- ncol(result[1, ]) - 1
    p <- p/dim

    M <- array(matrix(nrow = p, ncol = dim, 0), c(p, dim, s), c(2, 1, 3))  #output structure

    names <- list()

    for (i in 1:s) {
        line <- result[i, ]
        name <- as.character(line[[1]])  #get the name of  the specimens
        names <- c(names, name)
        line <- line[, 2:length(line)]

        m_specimen <- matrix(nrow = p, ncol = dim, 0)
        elem = 1
        for (k in 1:p) {
            coord = 1
            while (coord <= dim) {
                m_specimen[k, coord] <- as.double(line[[elem]])
                elem <- (elem + 1)
                coord = (coord + 1)
            }
        }


        M[, , i] <- m_specimen
    }


    return(list(M, names))
}

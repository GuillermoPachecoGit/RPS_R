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
spatialmed_landmark <- function(X) {
    tol = 1e-09

    n <- nrow(X)
    p <- ncol(X)

    m <- matrix(nrow = 1, ncol = p, 0)
    A <- matrix(nrow = n, ncol = p, 0)

    w <- matrix(nrow = 1, ncol = n, 1)
    s <- matrix(nrow = 1, ncol = n, 0)
    aux <- matrix(nrow = 1, ncol = p, 0)
    auxant <- matrix(nrow = 1, ncol = p, 0)

    tdemu <- matrix(nrow = 1, ncol = p, 0)
    rdemu <- matrix(nrow = 1, ncol = p, 0)
    gamagama <- 0
    sensor <- 0

    A <- X


    aux <- apply(A, 2, mean)  #aux <- w%*%A/sum(w)

    h <- 1
    # print(A)

    while ((median(abs(aux - auxant)) > tol) & (h <= 1000)) {
        # print('imprimo s:') print(aux)
        # print('******************************************************') print('AUX: ') print(aux)
        for (k in 1:n) {

            # print('A[k,]') print(t(as.matrix(A[k,]))) print(norm(aux - t(as.matrix(A[k,])) ) )
            if (norm(aux - t(as.matrix(A[k, ]))) == 0) {
                s[1, k] <- tol
                sensor <- 1
            } else {
                s[1, k] <- w[1, k]/norm(aux - t(as.matrix(A[k, ])), "F")
            }

        }
        auxant <- aux

        tdemu <- s %*% A/sum(s)
        rdemu <- s %*% A
        gamagama <- min(1, sensor, norm(rdemu, "F"))
        # print('imprimo s luego del ciclo:') print(s)
        aux <- (1 - gamagama) * tdemu + as.double(gamagama * aux)
        # print('Aux luego de la multiplicacion: ') print(aux)
        h <- (h + 1)

    }
    m <- aux

    return(m)
}


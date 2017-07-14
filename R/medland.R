#' A matrix A (nxp) is received and the length of the median landmark is returned.
#'
#' @param A S-dimensional array of n * k, which represents the different specimens without centralize.
#' @param z Indicates the type of median to be returned:
#' 1 -> the median landmark length)
#' 2 -> the median interlandmark distance (Rholf & Slice 90)
#' 3 -> the sum of landmarks lengths 
#' @return  Based on input choice z, the corresponding median value from A is returned
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
medland <- function(A, z) {

    f <- nrow(A) #number of landmarks

    b <- matrix(nrow = (f * (f - 1))/2, ncol = 1, 0)
    aux <- matrix(nrow = f, ncol = 1, 0)
    k <- 1
    for (i in 1:f) {
        aux[i, 1] <- norm(t(as.matrix(A[i, ])), "F")  # Frobenius or L2 vector norm
        # print('comienzo con la fila: ') print(i)
        for (j in i + 1:f) {
            # print('comparo con la fila: ') print(j)
            if (j <= f) {
                # print(j)
                b[k, 1] <- norm(t(as.matrix(A[i, ])) - t(as.matrix(A[j, ])), "F")  # obtengo la distancia entre pares de landmarks
                k <- (k + 1)
            }
        }
    }

    if (z == 1) {
        s <- median(aux)
    }
    if (z == 2) {
        s <- median(b)
    }
    if (z == 3) {
        s <- sum(aux)
    }

    return(s)
}


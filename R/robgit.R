#' Resistant Procrustes Superposition in R (RPS_R),
#' a novel package for resistant morphometrics
#'
#' Morphometric studies use landmark coordinates to
#' extract shape information, and shape differences between individuals are
#' analyzed by performing a Procrustes superposition of the corresponding
#' configurations of landmarks.
#' Which superposition criterion should be used is always a central issue;
#' typically, the least squares Procrustes superposition is chosen. This method minimizes
#' the sum of squared distances across landmarks, but it is widely accepted that the result can be
#' misleading whenever shape differences are located in less than 50% of the
#' landmarks.
#' A resistant Procrustes superposition (Torcida et al. 2014) is probably the most
#' elegant and efficient alternative, perfectly superimposing those landmarks
#' exhibiting no variation in case they are more than 50%. Despite this appealing
#' and rather intuitive feature, the resistant approach is seldom used probably due
#' to the lack of a free & friendly implementation.
#' This R-package specifically implements an a set of descriptive tools to perform
#' a resistant shape analysis of 2D and 3D configurations of landmarks.
#'
#' The RPS_R package includes:
#'  *a generalized resistant Procrustes superposition (robgit.R)
#'  *a resistant distance to quantify the resulting shape differences, and
#'  *a resistant Multidimensional Scaling to obtain the corresponding ordination.
#'  The corresponding least-squares counterparts have also been implemented, and both approaches can be therefore applied to the same dataset in order to compare their results.
#'
#' @section Functions:
#'  deleteLandmark, univMDSrobust, univMDSeucl, distanciaCM, distanciaR, plotAllSpecimensRobust, readland.txtJ
#'
#' @docType package
#' @name 'Robgit'
#' @param X A s-dimensional array of n * k matrix, representing the different specimens to be analyzed
#' @param consenso A logical value that determines if the array consensus is returned.
#' @return s-dimensional array of n * k matrix, representing the different specimens after adjusting.
#'
#'
#' @examples
#' craniums <- readland.tps('DataSet/craneos2D.tps')
#' adjustment_craniums <- robgit2012(craniums, consenso = TRUE)
#' @author Guillermo Andres Pacheco
#' @exports
robgit <- function(X, consenso = FALSE) {
    adjustament = FALSE
    if (ncol(X[, , 1]) == 2) {
        X <- adjustment3D(X)
        adjustament = TRUE
    }
    tol <- 1e-09
    iterTotal <- 20
    Z <- X
    f = nrow(Z[, , 1])
    c = ncol(Z[, , 1])
    r = ncol(Z[, 1, ])
    res <- matrix(nrow = f, ncol = r, 0)
    resme <- matrix(nrow = 1, ncol = r, 0)
    restot <- 0
    auxres <- matrix(nrow = f, ncol = r, 0)
    auxresme <- matrix(nrow = 1, ncol = r, 0)
    mejora <- matrix(nrow = f + 1, ncol = r, 0)
    Y <- matrix(nrow = f, ncol = c, 0)
    W <- matrix(nrow = f, ncol = c, 0)
    R <- array(matrix(nrow = f, ncol = c, 0), c(f, c, r + 1), dimnames = NULL)
    X <- array(matrix(nrow = f, ncol = c, 0), c(f, c, r), dimnames = NULL)
    X0 <- X
    E <- matrix(nrow = f, ncol = f, 0)
    A <- matrix(nrow = f, ncol = f, 0)
    Aux <- array(matrix(nrow = f, ncol = c, 0), c(f, c, r), dimnames = NULL)
    H <- array(matrix(nrow = 3, ncol = 3, 0), c(3, 3, r), dimnames = NULL)
    p <- matrix(nrow = 1, ncol = r, 1)
    TR <- matrix(nrow = f, ncol = c, 0)
    X <- Z
    s <- 0
    X <- initialFocus(X, f, r)
    X <- adjustmentScale(X, r)
    Y <- spatialmed_config(X)
    for (k in 1:r) {
        for (i in 1:f) {
            res[i, k] <- norm(t(as.matrix(Y[i, ])) - t(as.matrix(X[i, , k])), "F")
        }
        resme[1, k] <- median(res[, k])
        resme[1, k] <- sum(res[, k])
    }
    residual <- sum(resme)
    residualant <- 0
    z <- 1
    Aux <- X
    conteo <- matrix(nrow = f + 1, ncol = r, 0)
    conteomed <- matrix(nrow = 1, ncol = r, 0)
    while ((z <= iterTotal) & (medland(Y - W, 1) > tol)) {
        print(iterTotal)
        for (k in 1:r) {
            Aux[, , k] <- scaleSpecimen(Aux[, , k], Y)
            list_out <- rotation(Aux[, , k], H[, , k], Y)
            Aux[, , k] <- list_out[[1]]
            H[, , k] <- list_out[[2]]
            Aux[, , k] <- translation(Aux[, , k], Y, f)
            for (i in 1:f) {
                auxres[i, k] <- norm(t(as.matrix(Y[i, ])) - t(as.matrix(Aux[i, , k])), "F")
                if (auxres[i, k] <= res[i, k]) {
                  conteo[i, k] <- 1
                } else {
                  conteo[i, k] <- 0
                }
            }
            auxresme[1, k] <- median(auxres[, k])
            auxresme[1, k]
        }
        for (k in 1:r) {
            if (auxresme[1, k] <= resme[1, k]) {
                X[, , k] <- Aux[, , k]
                conteomed[1, k] <- 1
            } else {
                conteomed[1, k] <- 0
            }
        }
        conteo[f + 1, ] <- matrix(nrow = 1, ncol = r, 0)
        conteo[f + 1, ] <- (100 * apply(conteo, 2, sum))/f
        for (k in 1:r) {
            for (i in 1:f) {
                res[i, k] <- norm(t(as.matrix(Y[i, ])) - t(as.matrix(X[i, , k])), "F")
            }
            resme[1, k] <- median(res[, k])
        }
        W <- Y
        Y <- spatialmed_config(X)
        z <- (z + 1)
        iterTotal <- iterTotal - 1;
    }
    if (adjustament == TRUE) {
        V <- array(matrix(nrow = f, ncol = 2, 0), c(f, 2, r + 1), dimnames = NULL)
        for (i in 1:r) {
            U <- X[, , i]
            U <- U[, -3]
            V[, , i] <- U
        }
        U <- Y
        U <- U[, -3]
        if (consenso) {
            V[, , r + 1] <- U
            return(V)
        } else {
            return(V[, , -(r + 1)])
        }

    } else {
        R[, , 1:r] <- X
        if (consenso) {
            R[, , r + 1] <- Y
            return(R)
        } else {
            return(R[, , -(r + 1)])
        }

    }

}

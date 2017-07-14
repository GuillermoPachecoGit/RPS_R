#' This function obtains the initial estimate of the resistant rotation matrix R
#' by computing the rotation that leaves the triangle defined by points 0 (zero), 
#' Li (Landmark i) and Lj (Landmark j) in matrix X with the same orientation that the
#' corresponding triangle in the consensus matrix Y
#'
#' @param  X A nxk input matrix, representing an object
#' through n landmarks in k dimensions (k=2 or 3)
#' @param  Y The consensus or reference matrix
#' @param  i Landmark i
#' @param  j Landmark j
#'
#' @return The initial estimate of the kxk rotation matrix R, obtained through 
#' landmarks i and j 
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine & Sebastián Torcida
estimorot3D <- function(X, Y, i, j) {
    tol_norm = 1e-11
    dim <- ncol(X)
    A <- matrix(nrow = dim, ncol = dim, 0)
    B <- matrix(nrow = dim, ncol = dim, 0)
    v <- matrix(nrow = 1, ncol = dim, 0)
    w <- matrix(nrow = 1, ncol = dim, 0)
    a3 <- matrix(nrow = 1, ncol = dim, 0)
    b3 <- matrix(nrow = 1, ncol = dim, 0)

    if (i == j) {
        R = diag(3)
    } else {
        A[1, ] = (X[j, ] - X[i, ])/norm(t(as.matrix(X[j, ])) - t(as.matrix(X[i, ])), "F")  #tomo dos landmarks de l mismo especimen
        B[1, ] = (Y[j, ] - Y[i, ])/norm(t(as.matrix(Y[j, ])) - t(as.matrix(Y[i, ])), "F")

        v <- t(as.matrix(vector.cross(A[1, ], X[j, ])))
        w <- t(as.matrix(vector.cross(B[1, ], Y[j, ])))

        normav <- norm(v, "F")
        normaw <- norm(w, "F")

        if ((normav > tol_norm) & (normaw > tol_norm)) {
            A[2, ] <- v/normav
            B[2, ] <- w/normaw
            norm(as.matrix(A[2, ]), "F")
            a3 <- t(as.matrix(vector.cross(A[1, ], A[2, ])))
            A[3, ] <- a3/norm(a3, "F")
            b3 <- t(as.matrix(vector.cross(B[1, ], B[2, ])))
            B[3, ] <- b3/norm(b3, "F")

            R <- t(A) %*% B

        } else if ((normav < tol_norm) & (normaw < tol_norm)) {
            v <- t(as.matrix(vector.cross(A[1, ], B[1, ])))
            w <- v
            normav <- norm(v, "F")

            if (normav < tol_norm) {
                R <- diag(3)
            } else {
                A[2, ] <- v/normav
                B[2, ] <- A[2, ]

                a3 <- t(as.matrix(vector.cross(A[1, ], A[2, ])))
                A[3, ] <- a3/norm(a3, "F")
                b3 <- t(as.matrix(vector.cross(B[1, ], B[2, ])))
                B[3, ] <- b3/norm(b3, "F")


                R <- t(A) %*% B  #we are aligning planes through their orthogonal vectors!!
            }
        } else {
            R <- diag(3)
        }

    }

    return(R)

}

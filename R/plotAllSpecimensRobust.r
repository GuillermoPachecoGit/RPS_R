#' Plot landmark coordinates for all specimens
#'
#' Function plots landmark coordinates for a set of specimens
#'
#' The function creates a plot of the landmark coordinates for all specimens. This is useful for examining
#'  patterns of shape variation after GPA. If 'mean=TRUE', the mean shape will be calculated and added to the plot.
#'  Additionally, if a matrix of links is provided, the landmarks of the mean shape will be connected by lines.
#'  The link matrix is an m x 2 matrix, where m is the desired number of links. Each row of the link matrix
#'  designates the two landmarks to be connected by that link. The function will plot either two- or
#'  three-dimensional data (e.g. see \code{\link{define.links}}).
#'
#' @param A An array (p x k x n) containing GPA-aligned coordinates for a set of specimens
#' @param mean A logical value indicating whether the mean shape should be included in the plot
#' @param links An optional matrix defining for links between landmarks (only if mean=TRUE)
#' @param label A logical value indicating whether landmark numbers will be plotted (only if mean=TRUE)
#' @param plot.param A list of plotting parameters for the points (pt.bg, pt.cex), mean (mean.bg, mean.cex), links (link.col, link.lwd, link.lty) and landmark labels (txt.cex, txt.adj, txt.pos, txt.col)
#' @export
#' @keywords visualization
#' @author Dean Adams
#' @examples
#' data(plethodon)
#' Y.gpa<-gpagen(plethodon$land)    #GPA-alignment
#'
#' plotAllSpecimens(Y.gpa$coords,links=plethodon$links)
#' @exports
plotAllSpecimensRobust <- function(A, mean = TRUE, links = NULL, label = FALSE, plot.param = list(),
    pt.colors) {
    library("geomorph", lib.loc = "Packages/")
    library("igraph", lib.loc = "Packages/")
    if (length(dim(A)) != 3) {
        stop("Data matrix not a 3D array (see 'arrayspecs').")
    }
    if (any(is.na(A)) == T) {
        stop("Data matrix contains missing values. Estimate these first (see 'estimate.missing').")
    }
    k <- dim(A)[2]
    if (mean == TRUE) {
        mn <- mshape(A)
    }

    p.p <- plot.param
    if (is.null(p.p$pt.bg))
        p.p$pt.bg = "gray"
    if (is.null(p.p$pt.cex))
        p.p$pt.cex = 1
    if (is.null(p.p$mean.bg))
        p.p$mean.bg = "black"
    if (is.null(p.p$mean.cex))
        p.p$mean.cex = 2
    if (is.null(p.p$link.col))
        p.p$l.col = "black"
    if (is.null(p.p$link.lwd))
        p.p$link.lwd = 2
    if (is.null(p.p$link.lty))
        p.p$link.lty = 1
    if (is.null(p.p$txt.adj))
        p.p$txt.adj = c(-0.1, -0.1)
    if (is.null(p.p$txt.col))
        p.p$txt.col = "black"
    if (is.null(p.p$txt.cex))
        p.p$txt.cex = 0.8
    if (is.null(p.p$txt.pos))
        p.p$txt.pos = 1

    if (k == 2) {
        for (i in 1:dim(A)[[3]]) {
            if (i == 1) {
                plot(A[, 1, i], A[, 2, i], asp = 1, pch = 21, bg = pt.colors[i], cex = p.p$pt.cex *
                  1, xlab = "x", ylab = "y")
            } else {
                points(A[, 1, i], A[, 2, i], asp = 1, pch = 21, bg = pt.colors[i], cex = p.p$pt.cex *
                  1, xlab = "x", ylab = "y")
            }
        }


        if (label == TRUE) {
            text(mn, label = paste(1:dim(mn)[1]), adj = (p.p$txt.adj + p.p$mean.cex), pos = p.p$txt.pos,
                cex = p.p$txt.cex, col = p.p$txt.col)
        }
    }

    if (k == 3) {
        A3d <- NULL


        for (i in 1:dim(A)[[3]]) {
            if (i == 1) {
                plot3d(A[, , i], type = "s", col = pt.colors[i], xlab = "x", ylab = "y", zlab = "z",
                  size = p.p$pt.cex * 1.5, aspect = FALSE)
            } else {
                plot3d(A[, , i], type = "s", col = pt.colors[i], xlab = "x", ylab = "y", zlab = "z",
                  size = p.p$pt.cex * 1.5, aspect = FALSE, add = TRUE)
            }
        }


        if (label == TRUE) {
            text3d(mn, texts = paste(1:dim(mn)[1]), adj = (p.p$txt.adj + p.p$mean.cex), pos = p.p$txt.pos,
                cex = p.p$txt.cex, col = p.p$txt.col)
        }
    }
}

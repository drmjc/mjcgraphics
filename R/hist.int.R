#' Histogram of integers
#' Plot a histogram of the integers in \code{x}. Useful if \code{x} is
#'  a vector of counts or integers. 
#' It essentially is a smart \code{\link{hist}} that works out where to place the
#' breaks so that you can see the counts at each integer in \code{x}
#' @param x an integer vector
#' @param xlab the x-axis title
#' @param col a colour to be used to fill the bars.  The default of \code{NULL}
#'  yields unfilled bars.
#' @param add.density logical: add a density trace over the top?
#' @param density.adjust see \code{adjust} parameter to \code{\link{density}}
#' @param \dots arguments passed to \code{\link{hist}}
#' @author Mark Cowley, 16 May 2006
#' @export
hist.int <- function(x, xlab="counts", col=NULL,
                        add.density=FALSE, density.adjust=7, ...) {
    range <- range(x) + c(0, 1) - 0.5
    breaks <- seq(range[1], range[2], by=1)

    hist(x, breaks=breaks, xlab=xlab, col=col, xaxt="n", ...)
    axis(side=1, at=min(x):max(x))

    if( add.density ) {
        d <- density(x, adjust=density.adjust)
        h <- hist(x, breaks=breaks, plot=FALSE)
        d$y <- scale2(d$y, min=0, max=max(h$counts))
        if( is.null(col) )
            col <- "white"
        lines(d, col=inverse.colour(col))
    }
}

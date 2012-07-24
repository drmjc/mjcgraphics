#' Plot a matrix(-like object) row-wise.
#' 
#' Can specify a different col, lty and/or lwd for each line (defaults are all
#' 1).
#' \code{unique.lines} will work out a unique combination of col and lty for each row
#' of x (num rows <= 64)
#' 
#' @param x a matrix-like object of data, with each row to be plotted in a
#'   single line (if type draws lines)
#' @inheritParams graphics::plot
#' @inheritParams graphics::par
#' @param auto.log if the data is in the range [0,1] then it can be converted to
#'               -log10. Useful if x contains p-values.
#' @param symmetrical If the data in x is centred approx about 0, then setting
#'              to \code{TRUE} makes the plots a little more pleasing.
#' @param unique.lines works out a unique combination of line colour and type
#'                   (col and lty) such that the lines plotted are as unique
#'                   as possible (ie 8 coloured solid lines, then 8 dotted etc)
#' @param lty.cor If x contains records that are correlated, and perhaps anti-
#'              correlated, setting to TRUE will work out which lines are anti-
#'              correlated, and plot them as dashed lines, with all of the
#'              correlated lines drawn as solid lines.
#' @param legend.pos see \code{\link[graphics]{legend}}(pos)
#' 
#' @return none -- used to plot.
#' @author Mark Cowley, 29 August 2005
#' @export
plot.matrix <- function(x, bg.col="lightgrey",
                        main=NULL, sub=NULL,
                        ticks=c(1,2,3,4),
                        xlab="", xlim=c(1,ncol(x)), 
                        xlabels=rep("", ncol(x)),
                        ylab="", ylim=range(x, na.rm=TRUE), ylabels=NULL,
                        vablines=NULL, vabline.col="darkgrey", vabline.lty=1,
                        hablines=NULL, habline.col="darkgrey", habline.lty=1,
                        col=1, lty=1, lwd=1, pch=1, type="l", axes=TRUE, tcl=0.25,
                        add=FALSE,
                        auto.log=TRUE, symmetrical=FALSE,
                        unique.lines=FALSE, lty.cor=FALSE, 
                        legend.pos="none", ...) {

    if( unique.lines && lty.cor )
        stop("Can't specify both unique.lines and lty.cor\n")

    if(is.data.frame(x))
        x <- as.matrix(x)

    if(auto.log && max(x, na.rm=TRUE) <= 1.0 && all(x>0)) {
        cat("NB: data automatically transformed using -log10. Set auto.log=FALSE to disable\n")
        x <- -log10(x)
    }

    ## set up col, lty and lwd as appropriate (ie the same for all rows or as specified)
    if(unique.lines) {
        col <- rep(1:8, times=ceiling(ncol(x)/8))[1:ncol(x)]
        lty <- rep(1:ceiling(ncol(x)/8), each=8)[1:ncol(x)]
    }
    else if( lty.cor ) {
        #
        # Change the line type depending on whether the lines are correlated, or
        # anti-correlated with each other
        #
        tmpcor <- cor(t(x))
        diag(tmpcor) <- 0
        lty <- rep(1, nrow(x))
        lty[rowMeans( tmpcor ) < 0] <- 2 # dashed lines if they're -vely correlated

        # If there's only 2 lines to plot, and they're anti-correlated, then make one of them
        # have a solid line, and the other a dashed line.
        if( length(lty) == 2 && all(lty == 2) )
            lty[1] <- 1
    }
    else {
        # recycle lty to suit the number of rows in x
        if(length(lty) < nrow(x))
            lty <- recycle(lty, nrow(x))
    }

    # recycle col
    if(length(col) < nrow(x))
        col <- recycle(col, nrow(x))

    # recycle lwd.
    if(length(lwd) < nrow(x))
        lwd <- recycle(lwd, nrow(x))

    # recycle pch (if type="b")
    if(length(pch) < nrow(x))
        pch <- recycle(pch, nrow(x))

    if( symmetrical ) {
        ylim <- symmetricise( range(x, na.rm=TRUE) )
    }

    if( !add ) {
        plot2(x[1,], bg.col=bg.col,
              main=main, sub=sub,
              ticks=ticks,
              xlab=xlab, xlim=xlim, xlabels=xlabels,
              ylab=ylab, ylim=ylim, ylabels=ylabels,
              vablines=vablines, vabline.col=vabline.col, vabline.lty=vabline.lty,
              hablines=hablines, habline.col=habline.col, habline.lty=habline.lty,
              col=col[1], lty=lty[1], lwd=lwd[1], type=type, axes=axes, tcl=tcl, pch=pch[1], ...)

    }
    if( nrow(x) > 1 && type != "n" ) {
        for(i in c((2-add):nrow(x))) {
            points(x[i,], col=col[i], lty=lty[i], lwd=lwd[i], type=type, pch=pch[i], ...)
        }
    }

	if( legend.pos != "none" ) {
		legend(legend.pos, rownames(x), lty=lty, pch=pch, col=col, inset=0.02)
	}
}
# CHANGELOG:
# 2005-04-03: updated to reflect new plot2 arguments.

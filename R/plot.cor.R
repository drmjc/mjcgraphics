#' Correlation xy-plot
#' 
#' Plot x vs y with a lowess line and a correlation label under main title, and histograms
#' above and to the right of the plot.
#' 
#' @param x the 2 data to correlate
#' @param y the 2 data to correlate
#' @param symmetrical logical: make the plot symmetrical? (TRUE)
#' @param axes logical: add internal axes at x=0, y=0
#' @param lowess logical: add a lowess line through your data (default settings)
#' @param leastsquares logical: plot the straight line of least squares (see lm)
#' @param unity logical: plot a line through 0 with a slope of 1
#' @param hist logical: add histograms to the sides?
#' @param labels logical: label the data points with names to the right of them? This
#'   will also resize the axes by 110\% to let the right most points' names be
#'   seen.
#' @param cor.label logical: add a correlation label?
#' @param r2.label logical: add an r2 label?
#' @param cor.method the method to use to assess correlation. one of \dQuote{spearman} or
#'    \dQuote{pearson}
#' @param legend position of the legend. default=\dQuote{topleft}, see \code{\link{legend}}
#' @param xlim set the x-axis limits. leave as \code{NULL} to automatically determine.
#' @param ylim set the y-axis limits. leave as \code{NULL} to automatically determine.
#' @param \dots arguments passed to \code{\link{plot}}, or, if \code{hist=TRUE} 
#'    \code{\link{xyplot.hist}}
#' @return none.
#' @author Mark Cowley, 14 Nov 2006
#' @export
plot.cor <- function( x, y, symmetrical=TRUE, axes=TRUE,
                      lowess=TRUE, leastsquares=TRUE, unity=TRUE, hist=TRUE,
                      labels=NULL, cor.label=TRUE, r2.label=TRUE, cor.method="spearman", legend="topleft", 
                      xlim=NULL, ylim=NULL, ... ) {

    na2rm <- unionN( which(is.na(x)), which(is.na(y)) )
    if( length(na2rm) > 0 ) {
        x <- x[-na2rm]
        y <- y[-na2rm]
        if( !is.null(labels) )
            labels <- labels[-na2rm]
    }

	if( is.null(xlim)) {
	    xlim <- range(x)
	    if( symmetrical ) {
	        xlim <- c(min(c(min(x, na.rm=TRUE),min(y, na.rm=TRUE))), max(c(max(x, na.rm=TRUE),max(y, na.rm=TRUE))))
	        xlim <- symmetricise( xlim )
	    }

	    if( !is.null(labels) ) {
	        xlim[2] <- xlim[2] + abs(diff(xlim)*0.08)
	    }
	}
	if( is.null(ylim)) {
	    ylim <- range(y)
	    if( symmetrical ) {
	        ylim <- c(min(c(min(x, na.rm=TRUE),min(y, na.rm=TRUE))), max(c(max(x, na.rm=TRUE),max(y, na.rm=TRUE))))
	        ylim <- symmetricise( ylim )
	    }

	    if( !is.null(labels) ) {
	        if( symmetrical )
	            ylim[2] <- ylim[2] + abs(diff(ylim)*0.08)
	    }
	}

	if( hist )
    	xyplot.hist(x, y, symmetrical=ifelse(symmetrical, "xy", ""), ...)
	else
    	plot(x, y, xlim=xlim, ylim=ylim, ...)

    r <- cor(x,y)
    r2 <- r * r
    rho <- cor(x,y, method=cor.method)
    p <- cor.test(x,y,method=cor.method)$p.value
    cor <- paste("R^2:", signif(r2, 2), ifelse(cor.method=="spearman", "rho:", "cor:"), signif(rho, 2), "P:", signif(p, 2))
    cat(cor, "\n")
    if( cor.label ) {
        mtext(cor, side=3)
    }

    if( r2.label ) {
        legend("topleft", paste("R^2 =", signif(r2, 2)), bty="n")
    }

    if( axes )
        abline(h=0, v=0, lty=2, col="black")

    if( lowess )
        lines(lowess(x,y), col="purple")

    if( leastsquares )
        abline(lm(y ~ x, as.data.frame(cbind(x=x, y=y))), col="blue")

    if( unity )
        abline(0, 1, lty=3, col="darkgrey")

    if( !is.null(labels) )
        text(x,y,labels=labels, pos=4)
        
    if( !is.null(legend) ) {
        leg <- c("least-squares", "unity", "lowess", "axes")
        lty <- c(1, 3, 1, 2)
        col <- c("blue","darkgrey", "purple", "black")
        idx <- c(leastsquares, unity, lowess, axes)
        legend(x=legend, legend=leg[idx], lty=lty[idx], col=col[idx], bty="n", inset=0.01+r2.label*0.02)
    }

}
# CHANGELOG:
# 2012-03-23: bug fix if symmetrical=FALSE
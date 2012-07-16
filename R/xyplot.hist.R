#' An xyplot with histograms
#' An xyplot with a difference -- also show the distributions of data on the
#' xaxis, or yaxis.
#' 
#' This plot makes a normal xy plot, but also has a horizontal, and a vertical
#' histogram, on the
#' top and side of the plot.
#' Original code from Addicted to R: 
#' \url{http://wiki.r-project.org/rwiki/doku.php?id=graph_gallery:graph78}
#' 
#' @param x vectors of numbers, can contain NA's
#' @param y vectors of numbers, can contain NA's
#' @param breaks The number of breaks in the histograms. see ?hist
#' @param xlab see ?par. main will be plotted in the top centre of the plot
#' @param ylab see ?par. main will be plotted in the top centre of the plot
#' @param main see ?par. main will be plotted in the top centre of the plot
#' @param symmetrical make the axes symmetrical about zero. allowed values
#'   include \dQuote{} (no change), \dQuote{x}, make x-axis symmetrical \dQuote{y},
#'    make y-axis
#'   symmetrical \dQuote{xy}, make both x and y-axes symmetrical \dQuote{both},
#'  to make the plot square and symmetrical
#' @param axes logical: plot the x=0, y=0 axes as dashed lines
#' @param xhist.plot plot histograms of data on the x or yaxis? If both are
#'   \code{FALSE}, then just use xyplot.
#' @param yhist.plot plot histograms of data on the x or yaxis? If both are
#'   \code{FALSE}, then just use xyplot.
#' @param xhist.axis logical: add an axis for the x-histogram
#' @param yhist.axis logical: add an axis for the y-histogram
#' @param hist.col the colour for the histograms
#' @param xhist.col the colour for the histograms
#' @param yhist.col the colour for the histograms
#' @param \dots further arguments to \code{\link{xyplot}}.
#' @author Mark Cowley, 2008-09-02
#' @export
xyplot.hist <- function(x, y, breaks=50, xlab="", ylab="", main="", 
		symmetrical="", axes=TRUE, 
		xhist.plot=TRUE, yhist.plot=TRUE,
		xhist.axis=FALSE, yhist.axis=FALSE,
		xhist.lab="Frequency", yhist.lab="Frequency",
		hist.col="white", xhist.col=hist.col, yhist.col=hist.col, ...) {
	def.par <- par(no.readonly = TRUE) # save default, for resetting...

	# x <- pmin(3, pmax(-3, rnorm(50)))
	# y <- pmin(3, pmax(-3, rnorm(50)))
	xrange <- range(x, na.rm=TRUE)
	yrange <- range(y, na.rm=TRUE)
	if( grepl("x", symmetrical) )
		xrange <- symmetricise(xrange)
	if( grepl("y", symmetrical) )
		yrange <- symmetricise(yrange)
	if( symmetrical[1] == "both" ) {
		xrange <- yrange <- symmetricise(range(c(xrange,yrange)))
	}

	xhist <- hist(x, breaks=breaks, plot=FALSE)
	yhist <- hist(y, breaks=breaks, plot=FALSE)
	top <- max(c(xhist$counts, yhist$counts))
	# nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
	# layout.show(nf)
	if( xhist.plot && yhist.plot )
		nf <- layout(matrix(c(1,0,3,2),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
	else if( xhist.plot && !yhist.plot )
		nf <- layout(matrix(c(1,2),2,1),heights=c(1,3), respect=FALSE)
	else if( !xhist.plot && yhist.plot )
		nf <- layout(matrix(c(2,1),1,2),widths=c(3,1),respect=FALSE)
	# ^ do the main xy plot last so that it is still active, and thus more
	# lines etc can be added after this plotting function.
	
	
	# provide space to print a plot title
	if( !is.null(main) && main != "" )
		par(oma=c(1,0,3,0))

	###########################################
	#v1 doesn't handle symmmetricising
	# par(mar=c(0,3,1,1))
	# barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
	# par(mar=c(3,0,1,1))
	# barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
	# par(mar=c(4,4,1,1))
	# plot(x, y, xlim=xrange, ylim=yrange, xlab=xlab, ylab=ylab, main="", ...)
	###########################################


	###########################################
	# v2 doesnt handle horizontal histogram
	# par(mar=c(0,3,1,1))
	# hist(x, breaks=breaks, axes=FALSE, ylim=c(0, top), xlim=xrange, col="lightgrey", main="")
	# par(mar=c(3,0,1,1))
	# hist(y, breaks=breaks, axes=FALSE, xlim=c(0, top), ylim=yrange, col="lightgrey", horiz=TRUE)
	# par(mar=c(4,4,1,1))
	# plot(x, y, xlim=xrange, ylim=yrange, xlab=xlab, ylab=ylab, main="", ...)
	###########################################

	###########################################
	# v3, allows x and y shifts.
	xoffset <- min(x, na.rm=TRUE) - xrange[1]
	xhistlim <- xrange + (0 - xrange[1]) - xoffset
	xwidth <- xhist$breaks[2] - xhist$breaks[1]
	# xhistlim <- xhistlim - xwidth/2

	yoffset <- min(y, na.rm=TRUE) - yrange[1]
	yhistlim <- yrange + (0 - yrange[1]) - yoffset
	ywidth <- yhist$breaks[2] - yhist$breaks[1]
	yhistlim <- yhistlim - ywidth/2 # NO idea why this works....

	par(pty="m") # if pty == "s" and is thus square, the histograms are the wrong size.
	if( xhist.plot ) {
		suppressWarnings(par(mar=c(0,3.5,0.5,0.5),mgp=c(2,0.75,0)))
		barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, width=xwidth, xlim=xhistlim, col=xhist.col, ylab=ifelse(xhist.axis,xhist.lab,""))
		if( xhist.axis == TRUE ) {
			axis(side=2)
		}
	}
	if( yhist.plot ) {
		suppressWarnings(par(mar=c(3.5,0,1,0.5),mgp=c(2,0.75,0)))
		barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, width=ywidth, ylim=yhistlim, horiz=TRUE, col=yhist.col, xlab=ifelse(yhist.axis,yhist.lab,""))
		if( yhist.axis == TRUE )
			axis(side=1)
	}
	par(mar=c(3.5,3.5,1,0.5), mgp=c(2,0.75,0))
	plot(x, y, xlim=xrange, ylim=yrange, xlab=xlab, ylab=ylab, main="", ...)
	###########################################
	
	# add the main title to the top middle of the panel.
	if( !is.null(main) )
		mtext(side=3, outer=TRUE, main, line=1, cex=1.2)

	# par(def.par)
}

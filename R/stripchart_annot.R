#' Make a normal stripchart, but below the plot add a specified value for each
#' set of points (such as the mean or MAD or stdev)
#' 
#' @inheritParams graphics::stripchart
#' @param values the vector of values to annotate with
#' @param vlab the value label which is plotted on \code{axis(side=2)}
#' @param val.ratio the heights ratio, relative to the main plot which has a value of 1.
#'  default=0.1, ie the bottom annotation plot will be 10\% of the size
#'  of the main plot
#' @param frame logical: add lines around each bar?
#' 
#' @return none. makes a plot.
#' 
#' @author Mark Cowley, 2009-07-31
#' @export
#' @aliases stripchart_annot stripchart.annot
stripchart_annot <- function(x, values, vlab="stat", val.ratio=0.1, frame=TRUE,
	method="jitter", jitter=0.2, offset=1/3, vertical=TRUE, group.names, ylim=NULL, ylab=NULL, xlab=NULL, pch=0, do.layout=TRUE, las=2, yaxt="s", group.median=FALSE, group.mean=FALSE, ...) {

	opar <- par(no.readonly=TRUE)[c("mar", "oma", "usr")]
	# on.exit(par(opar))

	mar <- par()$mar
	if( is.logical(do.layout) ) {
		if( do.layout ) {
			layout(matrix(c(1,2),nrow=2), heights=c(1,val.ratio))
			par(oma=c(mar[1],0,0,0))
		}
	}
	else if ( is.numeric(do.layout) ) {
		mat <- matrix(1:(2*do.layout), nrow=2, byrow=FALSE)
		layout(mat, heights=c(1,val.ratio))
	}

	par(mar=c(0,mar[2:4]))
	stripchart(x, method=method, jitter=jitter, vertical=TRUE, ylab=ylab, pch=pch, ylim=ylim, 
		xaxt="n", las=las, xaxs="i", yaxt=ifelse(yaxt=="t", "n", yaxt), ...)
	if( frame )
		abline(v=1:(ncol(x)-1)+0.5, lty="dashed", col='grey')
	if( yaxt=="t" ) # ticks axis
		axis(side=2, labels=FALSE)
	
	usr <- par()$usr
	
	if( group.median ) {
		med <- apply(x, 2, median, na.rm=TRUE)
		points(1:ncol(x), med, pch=95, cex=2)
	}
	if( group.mean ) {
		med <- apply(x, 2, mean, na.rm=TRUE)
		points(1:ncol(x), med, pch=95, cex=2)
	}
	
	par(mar=c(0, mar[2], 0, mar[4]))
	plot.new()
	par(usr=c(usr[1:2], 0, 1))

	#
	# the midpoints are where the ticks, axis labels, and values should be drawn
	#
	mids <- 1:ncol(x)
	if( missing(group.names) && !is.null(colnames(x)) )
		group.names <- colnames(x)

	if( !is.null(group.names) )
		axis(side=1, at=mids, tick=TRUE, group.names, las=las)

	# frame the values
	box()
	mids2 <- mids[1:(length(mids)-1)]
	abline(v=mids2 + (mids2[2] - mids2[1])/2)
	# add the values and a value label
	text(mids, rep(0.5,length(values)), prettyNum(values))
	axis(side=2, at=0.5, tick=FALSE, vlab, las=2)
}


#' Density plots for every column in a matrix
#' 
#' Generic function to plot the columns of a \code{matrix}/\code{data.frame} as overlayed
#' density plots.
#' 
#' @param data a \code{matrix} or \code{data.frame} of values
#' @param legend logical: add a legend to the plot?
#' @param add logical: if \code{TRUE}, add to an existing plotting device, else create 
#'    a new plotting device
#' @param main the plot title
#' @param ylim the y-axis limits. if \code{NULL}, this is auto-calculated
#' @param xlim the x-axis limits. if \code{NULL}, this is auto-calculated
#' @param xlab the x-axis title
#' @param ylab the y-axis title
#' @param col the colours of each density curve, which are recycled if necessary. if \code{NULL}, 
#'    the standard colours \code{1:8} are used.
#' @param auto.log2 logical: automatically log-base-2 transform
#' @param lty the line type for each density play. if \code{NULL}, line types \code{1:ncol(data)}
#' @param legend.cex the character expansion of the legend. default=0.5
#' @param \dots arguments passed to \code{\link{lines}}
#' @author Mark Cowley, 9 May 2005
#' @export
plot.density.matrix <- function(data, legend=FALSE, add=FALSE, main="Density Plot", ylim=NULL, xlim=NULL, 
     xlab="Expression Level (Log2)", ylab="Probability Density", col=NULL, auto.log2=TRUE, lty=NULL, legend.cex=0.5, ...) {
	if( is.vector(data) ) {
		## Then convert the vector to a matrix with 1 column
		data <- matrix(data, ncol=1)
	}
	if( auto.log2 && max(data[,1], na.rm=TRUE) > 1000 ) {
		cat("Automatically transforming data by log2...")
		data <- log2(data)
		cat("\n")
	}

	# pre-calc the densities as a list
	d <- apply(data, 2, density.sketch, sketch=10000, na.rm=TRUE)

	## determine the colour of each density curve
	if( length(col) == 0 )
		col <- 1:ncol(data)
	else
		col <- recycle(col, ncol(data))
	## determine the lty of each density curve
	if( length(lty) == 0 )
		lty <- 1:ncol(data)
	else
		lty <- recycle(lty, ncol(data))
	
	
	
    if( add )
		for( i in 1:ncol(data) )
			##lines(density(data[,i], na.rm=TRUE), col=col[i], ...)
			lines(d[[i]], col=col[i], lty=lty[i], ...)
    else {
		## Determine the ylim that will allow all of the density curves to be shown
		if( is.null(ylim) )
			## ylim <- c(0, max(apply(data, 2, function(x) return(max(density(x, na.rm=TRUE)$y)) ) ) )
			ylim <- c(0, max(unlist(lapply(d, function(x) return(max(x$y)) ) )) )

		## Determine the xlim that will allow all of the density curves to be shown
		if( is.null(xlim) ) {
			xlim <- c( min(unlist(lapply(d, function(x) return(min(x$x))))),
			           max(unlist(lapply(d, function(x) return(max(x$x))))) )
		}

		plot(d[[1]], ylim=ylim, xlim=xlim, main=main, xlab=xlab, ylab=ylab, col=col[1], lty=lty[1], ...)

		if( ncol(data) > 1 ) {
			for( i in 2:ncol(data) )
				lines(d[[i]], col=col[i], lty=lty[i], type="l", ...)
		}

		if( legend ) {
			legend("topright", legend=colnames(data), lty=lty, col=col, ncol=ceiling(ncol(data)/25), cex=legend.cex, inset=0.01)
		}
			# legend.TR(fill=col, legend=colnames(data), cex=0.5)
	}

	# rm(d)

	# gc(verbose=FALSE)
}

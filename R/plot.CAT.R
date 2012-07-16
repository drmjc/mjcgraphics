#' Correspondence at the top (CAT) plot
#' 
#' CAT plots are a useful way of comparing 2 lists, over a dynamic range of \sQuote{top N}
#' thresholds.
#'
#' \code{xTrue}, \code{yTrue} and \code{pch}\cr
#' Often, some of the \code{x} and \code{y} values also have a measure of statistical significance, or interest,
#' and it would be nice to
#' know at which point along the resulting plot that (a) both \code{x} and \code{y} were statistically significant
#' (b), when neither are significant, and (c), where just x or y is significant. if \code{pch=NULL}, and
#' \code{xTrue} and \code{yTrue} contain a range of \code{TRUE} and \code{FALSE}, then:
#' if \code{xTrue & yTrue}, pch = 19 = filled circle
#' if \code{xTrue | yTrue}, pch = 1 = open circle
#' if \code{!xTrue & !yTrue}, pch = 2 = open triangle
#'
#' @note
#' To create a CAT plot we made a list of \code{n} candidate genes for each of the
#' two procedures and plotted the proportion of genes in common against the
#' list size \code{n} (Irizarry et al, Nature Methods 2, 345 - 350 (2005))
#' sketch: in [0,1]. if < 1 then only a subset of points will be plotted when
#' the sets are
#' larger than 1000 values to compare. ie, if sketch=0.1, then only 10% of the
#' data points
#' in 1001:end will be plotted. This over-rides the sizes paramter. Set it to
#' 1.0 to ignore it.
#' 
#' @param x usually a character vectors, but could be a vector of distinct numbers.
#' @param y usually a character vectors, but could be a vector of distinct numbers.
#' @param sizes an integer vector to use as the values for \dQuote{n}
#' @param sketch a value in (0,1.0] to be used if there are > 1000 datapoints
#' @param main see \code{\link{plot}}
#' @param ylim see \code{\link{plot}}
#' @param add see \code{\link{plot}}
#' @param proportion logical: set x-axis to proportion in common
#' @param xTrue logical vector, same length as \dQuote{x} to indicate significance of values in \dQuote{x}
#' @param yTrue logical vector, same length as \dQuote{y} to indicate significance of values in \dQuote{y}
#' @param pch the print character. Default: NULL = choose the pch based on the values for xTrue and yTrue. see Details
#' @param \dots arguments passed to plot
#' 
#' @return none. makes a plot.
#' 
#' @author Mark Cowley, 29 August 2006
#' @references 
#' Irizarry et al, Nature Methods 2, 345 - 350 (2005)
#' @export
#' @importFrom gtools permute
#' @seealso \code{\link{calc.CAT}}
plot.CAT <- function(x, y, sizes=1:length(x), sketch=0.1, 
	main="CAT plot", ylim=c(0,1), add=FALSE, proportion=FALSE, 
	xTrue=rep(TRUE, length(x)), yTrue=rep(TRUE, length(y)), pch=NULL, ...) {
	
	N <- min(length(x), length(y))
	
	if( sketch < 1.0 && N > 1000 ) {
		sizes <- c(1:1000, seq(1001, N, round(1/sketch)))
	}
	else {
		sizes <- sizes[sizes < N]		
	}
	# ensure N and sizes are in sync
	N <- max(sizes)

	overlaps <- calc.CAT(x,y,sizes)

	# cat(length(x), length(y), N,"\n")
	
	sizes <- sizes / N
	xlim <- c(0,1)
	if( proportion ) {
		# sizes <- sizes / N
		xlab <- "size of list (proportion of max)"
		# xlim <- c(0,1)
	}
	else {
		xlab <- "size of list"
		# xlim=c(1, N)
	}
	
	if( is.null(pch) ) {
		if( any(!c(xTrue,yTrue)) ) {
			# Then lets use mixed mode print characters where x and y are BOTH TRUE, just one is TRUE, or both are FALSE
			if( length(x) < length(y) )
				xTrue <- c(xTrue, rep(xTrue[length(xTrue)], length(y)-length(x))) # repeat the last value of xTrue X more times, to make it the same length as yTrue
			else if( length(x) > length(y) )
				yTrue <- c(yTrue, rep(yTrue[length(yTrue)], length(x)-length(y))) # repeat the last value of yTrue X more times, to make it the same length as xTrue

			pch <- rep(19, N) # filled circle
			pch[(xTrue + yTrue) == 1] <- 2 # triangle;   22 # square 
			pch[(xTrue + yTrue) == 0] <- 1 # open circle
		}
		else
			pch <- rep(1, N)
	}
	
	if( !add ) {
		plot(sizes, overlaps, xlab=xlab, ylab="Proportion in common", main=main, ylim=ylim, xlim=xlim, pch=pch, 
		xaxt=ifelse(proportion,"s", "n"), ...)
		axis(side=1, at=sizes, tcl=-0.2, tick=TRUE, labels=FALSE)
		if( !proportion ) {
			ticks <- pretty(1:N, min.n=5)
			at <- ticks/N
			axis(side=1, at=at, tick=TRUE, labels=ticks)
		}
	}
	else
		points(sizes, overlaps, pch=pch, ...)

	invisible(overlaps)
}

#' Calculate the overlap b/w two vectors, for various sizes.
#' 
#' see also plot.CAT
#' 
#' @param x usually 2 character vectors, but could be a vector of distinct
#'   numbers.
#' @param y usually 2 character vectors, but could be a vector of distinct
#'   numbers.
#' @param sizes a numeric vector of sizes at which to determine the overlap b/w
#'   x and y. eg c(1,5,10,100) will find out how many values in common b/w x
#'   and y for the top 1, 5, 10 or 1000 values in x.
#' @return a numeric vector corresponding to the proportion of values in common
#'   in x and y for each given size, named by the size used.  eg: if you take
#'   top 10 from each list, and 5 are in common, the proportion is 0.5
#' @author Mark Cowley, 2008-05-15
#' @export
#' @seealso \code{\link{plot.CAT}}
calc.CAT <- function(x,y,sizes) {
	res <- rep(0, length(sizes))
	for(i in 1:length(sizes)) {
		res[i] <- length(intersect(x[1:(sizes[i])], y[1:(sizes[i])]))/sizes[i]
	}
	names(res) <- sizes

	return( res )
}



#' Permutation based CAT plots.
#' 
#' Permute \code{x} or \code{y}, \code{B} times, and re-calculate the CAT traces of x vs y', or x'
#' vs y.
#' Plot this distribution of randomised CAT traces as a grey null distribution,
#' with the unpermuted CAT trace over the top.
#' 
#' @param x usually a character vectors, but could be a vector of distinct numbers.
#' @param y usually a character vectors, but could be a vector of distinct numbers.
#' @param B the number of permutations
#' @param sizes an integer vector to use as the values for \dQuote{n}
#' @param sizes.random an integer vector to use as the values for \dQuote{n}. default=\code{NULL}, which
#'   take on the first c(1:100,200,300,N)
#' @param ylim see \code{\link{plot}}
#' @param proportion logical: set x-axis to proportion in common
#' @param random.col the background colour of the randomised area
#' @param add see \code{\link{plot}}
#' @param col the colour of the foreground points
#' @param randomise which data set to randomise? \dQuote{x} or \dQuote{y}.
#' @param pch the print character. Default: NULL = choose the pch based on the values for xTrue and yTrue. see Details
#' @param main see \code{\link{plot}}
#' @param \dots arguments passed to plot
#' 
#' @author Mark Cowley, 2008-12-08
#' 
#' @export
#' @importFrom gtools permute
#' 
#' @seealso \code{\link{plot.CAT}}
plot.CAT.vs.random <- function(x, y, B=100, sizes=1:2000, 
	sizes.random=NULL, ylim=c(0,1), 
	proportion=FALSE, random.col="lightgrey", add=FALSE, col=1, randomise="y", pch=1, main="CAT plot", ...) {

	if( is.null(sizes) ) {
		if( length(x) > 100 )
			sizes <- c(1:100, seq(101, length(x),100), length(x))
		else
			sizes <- 1:length(x)
	}
	if( is.null(sizes.random) ) {
		if( length(x) > 100 )
			sizes.random <- c(1:100, seq(101, length(x),100), length(x))
		else
			sizes.random <- 1:length(x)
	}
	
	N <- max(sizes)
	
	if( length(x) > N )
		x <- x[1:N]
	if( length(y) > N )
		y <- y[1:N]

	tmp <- matrix(NA, nrow=length(sizes.random), ncol=B)
	for( i in 1:B ) {
		if( randomise == "y" ) tmp[,i] <- calc.CAT(x, permute(y), sizes=sizes.random)
		else tmp[,i] <- calc.CAT(x, permute(x), sizes=sizes.random)
	}
	minima <- apply(tmp, 1, quantile, 0.01)
	maxima <- apply(tmp, 1, quantile, 0.99)
	med <- apply(tmp, 1, median)
	# trace the outline of this polygon, from bottom right, clockwise
	sizes.random <- sizes.random/max(sizes.random) # x-axes for CAT plots are now always in [0,1]
	polygon.x <- c(sizes.random, rev(sizes.random))
	polygon.y <- c(minima, rev(maxima))
	
	# We don't want to plot the polygon over the top of the points from the CAT plot.
	# Thus, plot a blank CAT plot, then the polygon, then re-plot the CAT plot
	if( !add ) plot.CAT(x,y,sizes=sizes, ylim=ylim, add=FALSE, proportion=proportion, type="n", main=main, sketch=1, ...)
	polygon(polygon.x, polygon.y, col=random.col)
	lines(sizes.random, med, lty=4, lwd=2)
	plot.CAT(x,y,sizes=sizes, add=TRUE, proportion=proportion, col=col, pch=pch, sketch=1, ...)
}

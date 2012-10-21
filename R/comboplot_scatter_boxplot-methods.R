#' A combination plot: sorted scatter plot and boxplot
#' 
#' A 2 panel plot of a sorted numeric vector (from low to high), and a boxplot on the right.
#' Additionally, a percentiles axis is added to the top, and a grid is plotted if \code{do.percentiles=TRUE}.
#' Useful for plotting gene expression data for determining a cutoff.
#' 
#' @section input types:
#' \code{x} can be a \code{numeric vector}; \code{x} can be an \code{ExpressionSet}, with >=1 
#' features/genes (1 plot will be made per feature).
#'
#' @param x a \code{numeric vector}, or an \code{ExpressionSet} object
#' @param main see \code{\link{plot}}. If \code{is(x, "ExpressionSet")}, then leave \code{main=""}, and
#' the featureName will be used for each plot.
#' @param xlab see \code{\link{plot}}
#' @param ylab see \code{\link{plot}}
#' @param do.percentiles logical: if \code{TRUE}, then add a percentiles axis and grid to the plot
#' @param do.layout logical: if \code{TRUE}, then layout the plotting region. Set to \code{FALSE}
#'  if you'd rather do this once, first in a large loop.
#' @param ylim numeric(2) of the min and max on the y-axis.
#' @param \dots arguments passed to \code{\link{plot}}
#' 
#' @return nothing.
#' 
#' @author Mark Cowley, 2012-09-12
#' @export
#' @S3method comboplot_scatter_boxplot default
#' @S3method comboplot_scatter_boxplot numeric
#' @S3method comboplot_scatter_boxplot default
#' @rdname comboplot_scatter_boxplot-methods
#' @examples
#' if( interactive() ) {
#'   dat <- rnorm(100)
#'   comboplot_scatter_boxplot(dat)
#'   comboplot_scatter_boxplot(dat, do.percentiles=FALSE)
#' }
#' 
comboplot_scatter_boxplot <- function(x, main, xlab, ylab, do.percentiles, do.layout, ylim, ...) UseMethod("comboplot_scatter_boxplot")

comboplot_scatter_boxplot.default <- function(x, main="", xlab="Index", ylab="level", do.percentiles=TRUE, do.layout=TRUE, ylim=range(x, na.rm=TRUE), ...) {
	stop(paste("not yet implemented for class(x) of:", class(x)))
}

comboplot_scatter_boxplot.numeric <- function(x, main="", xlab="Index", ylab="level", do.percentiles=TRUE, do.layout=TRUE, ylim=range(x, na.rm=TRUE), ...) {
	opar <- par(no.readonly=TRUE)
	if( do.layout ) {
		layout(matrix(c(1,2),nrow=1),widths=c(8,1))
		par(mar=c(5.1,5.1,5.7,0))
	}
	plot(sort(x), xlab=xlab, ylab=ylab, main=main, ylim=ylim, ...)
	abline(h=quartile(x), lty="dashed", col="darkgrey")
	abline(h=mean(x), lty="longdash", col="darkgrey")
	if( do.layout ) par(mar=c(5.1,0.2,5.7,0.5))
	if( do.percentiles ) {
		axis.percentiles(side=3, max=length(x))
		grid.percentiles(side=3, max=length(x))
	}
	boxplot(as.data.frame(x), ylim=ylim, yaxt="n")
	abline(h=quartile(x), lty="dashed", col="darkgrey")
	abline(h=mean(x), lty="longdash", col="darkgrey")
	if( do.layout ) par(opar)
}


#' @S3method comboplot_scatter_boxplot ExpressionSet
#' @importClassesFrom Biobase ExpressionSet
#' @importFrom Biobase exprs featureNames
#' @rdname comboplot_scatter_boxplot-methods
comboplot_scatter_boxplot.ExpressionSet <- function(x, main="", xlab="Index", ylab="Expression level (log2)", do.percentiles=TRUE, do.layout=TRUE, ylim=range(exprs(x), na.rm=TRUE), ...) {
	for(i in 1:nrow(x)) {
		g <- x[i,]
		if( main == "" ) main <- featureNames(g)
		comboplot_scatter_boxplot(exprs(g)[1,,drop=T], main=main, xlab=xlab, do.percentiles=do.percentiles, do.layout=do.layout, ylim=ylim, ...)
	}
}

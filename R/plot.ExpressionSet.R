#' plot a single feature from an ExpressionSet
#' 
#' Plot a single probe, aka feature from an ExpressionSet object, optionally highlighting certain
#' samples, and adding a mean expression line.
#'
#' @param x an ExpressionSet
#' @param feature the index or name of the feature to plot. default=1
#' @param xlab see par
#' @param ylab see par
#' @param main see par
#' @param add.mean logical: add a horizontal line about the mean of that feature
#' @param sort logical: if \code{TRUE}, then sort from low to high
#' @param samples an optional character vector of sample names to highlight and label in red.
#' 
#' @return nothing
#' 
#' @author Mark Cowley, 2012-10-16
#' 
#' @export
#' @importFrom Biobase featureNames sampleNames
#' 
#' @examples
#' \dontrun{
#' 	hent3 <- x["SLC29A3", ]
#'	plot_ExpressionSet(hent3, sort=T, samples="APGI_1966")
#'	plot_ExpressionSet(x, feature="SLC29A3", sort=T, samples="APGI_1966")
#' }
plot_ExpressionSet <- function(x, feature=1, xlab="Rank", ylab="Expression Level (log2)", main=featureNames(x)[feature], add.mean=TRUE, sort=TRUE, samples=NULL) {
	o <- 1:ncol(x)
	if( sort ) {	
		o <- order(exprs(x)[feature,,drop=T], decreasing=FALSE)
	}
	plot(exprs(x)[feature,o,drop=T], xlab="Rank", ylab="Expression Level (log2)", main=main)
	if( add.mean ) {
		abline(h=rowMeans(exprs(x[feature, ])), col="red")
		text(1, rowMeans(exprs(x[feature, ])), "mean", col="red", pos=3)
	}
	if( !is.null(samples) ) {
		if( !all(samples %in% sampleNames(x)) ) stop("some samples not in sampleNames(x): ", setdiff(samples, sampleNames(x)))
		xpos <- o[which(sampleNames(x) == samples)]
		ypos <- exprs(x[feature,xpos])[1,,drop=T]
		points(xpos, ypos, pch=19, col="red")
		text(xpos, ypos, samples, col="red", pos=2)
	}
}
# CHANGELOG
# 2012-11-14: bug fix in add.mean which added 1 line per feature in x

#' xy-plot of data from 2 ExpressionSet's
#' Note only the first feature from each ExpressionSet are plotted.
#' 
#' @rdname plot-methods
#' @aliases plot,ExpressionSet,ExpressionSet-method
#' @importFrom stats lowess
#' @importMethodsFrom lumi plot
#' @export
setMethod("plot",
	signature=signature("ExpressionSet", "ExpressionSet"),
	function(x, y, loess.col=NA, ...) {
		common.names <- intersect(sampleNames(x), sampleNames(y))
		length(common.names) > 0 || stop("no sample names found in common")
		x <- x[,common.names]
		y <- y[,common.names]
		a <- exprs(x)[1,,drop=T]
		b <- exprs(y)[1,,drop=T]
		names(a) <- names(b) <- common.names
		
		has.data <- !is.na(a) & !is.na(b)
		
		plot(a, b, ...)
		title(sub=paste("n =", sum(has.data)), outer=FALSE)
		
		if( !is.na(loess.col) ) {
			lines(stats::lowess(a[has.data],b[has.data]), col=loess.col)
		}
	}
)

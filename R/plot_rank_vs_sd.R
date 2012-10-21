#' plot rank vs stdev
#' 
#' For a matrix of data (eg gene expression data) for each row (gene), plot the
#' rank of the mean vs the stdev of that row.
#' 
#' @param x either a numeric matrix/data.frame, or an AffyBatch, or LumiBatch
#'   object (from lumi package)
#' @param main the plot title
#' @param lowess.col the colour of the lowess line
#' 
#' @return none. makes a plot
#' 
#' @author Mark Cowley, 2009-05-27
#' @export
plot_rank_vs_sd <- function(x, main="", lowess.col="purple") {
	# cls <- class(x)
	# if ( cls %in% c("LumiBatch", "AffyBatch") ) {
	# 	x <- exprs(x)
	# }
	tmpSD <- rowSD(x)
	tmpRank <- rank(rowMeans(x))
	plot(tmpRank, tmpSD, xlab="rank(mean)", ylab="Standard deviation", main=main)
	lines(lowess(tmpRank, tmpSD), col=lowess.col, lwd=1.5)
}
# CHANGELOG:
# 2012-07-16: dropped the exprs call-logic to avoid a Biobase dependency in this plotting package.
# 2012-10-21: renamed from plot.rank.vs.sd to plot_rank_vs_sd

#' barplot a binary variable into different strata
#' 
#' Make a barplot of a binary variable, stratified by a certain function, like
#' which.percentile, or which.quantile. 
#' 
#' @param binary a boolean vector
#' @param strata an additional vector that describes which strata each point came
#' from, such as which percentile. see \code{\link[mjcbase]{which.percentile}}
#' @param proportion logical: y-axis as a proportion? Recommended to be TRUE
#' @param \dots arguments passed to \code{\link[graphics]{barplot}}
#' 
#' @return none. makes a plot
#' 
#' @author Mark Cowley, 2008-12-22
#' @export
barplot_stratum <- function(binary, strata, proportion=TRUE, ...) {
	levels <- sort(unique(strata))
	res <- rep(0, length(levels))
	
	names(res) <- as.character(levels)
	for(level in levels) {
		res[as.character(level)] <- sum(binary[strata==level])
		if( proportion )
			res[as.character(level)] <- res[as.character(level)] / sum(strata==level)
	}
	barplot(res, ...)
}

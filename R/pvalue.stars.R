# Determine how many star's to annotate a vector of pvalues
# 
# Parameters:
#	pvals: a vector of pvalues
# 	thresh: a vector of  thresholds for 1, 2, 3... stars. this can be as long as the user needs
# 
# Value:
# A character vector of "", "*", "**", ... same length as the input
#
# Mark Cowley, 2009-03-20
#


#' Determine how many star's to annotate a vector of pvalues
#' 
#' @param pvals a vector of pvalues
#' @param thresh a vector of thresholds for 1, 2, 3... stars. this can be as
#'   long as the user needs
#' @return A character vector of "", "*", "**", ... same length as the input
#' @author Mark Cowley, 2009-03-20
#' @export
pvalue.stars <- function(pvals, thresh=c(0.05, 0.01, 0.001)) {
	res <- rep("", length(pvals))
	for(i in 1:length(thresh)) {
		res[ pvals < thresh[i] ] <- paste(rep("*", i), collapse="")
	}
	res
}

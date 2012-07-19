# Allows use of brewer.pal with less than 3 colours
#
# Parameters:
#	n: Number of different colors in the palette, minimum 1, maximum depending on palette
#	name: A palette name from the lists at ?brewer.pal
#
# Value:
#	a vector of N colours
#
# Mark Cowley, 2010-09-02


#' Allows use of brewer.pal with less than 3 colours
#' 
#' @param n Number of different colors in the palette, minimum 1, maximum
#'   depending on palette
#' @param name A palette name from the lists at ?brewer.pal
#' @return a vector of N colours
#' @author Mark Cowley, 2010-09-02
#' @export
safe.brewer.pal <- function(n, name) {
	require(RColorBrewer)
	if (n<3) {
		brewer.pal(3, name)[(3-n):3] # choose the n colours at the end, since they go from less to more intense
	}
	else {
		brewer.pal(n, name)
	}
}

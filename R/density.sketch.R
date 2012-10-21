#' density sketch
#' 
#' For huge vectors, density can take a very long time.
#' 
#' Inspired by RMA-sketch which takes a subset of data points to determine
#' the reference distribution, this method computes the density on a subset of
#' data points from the original vector.
#' 
#' @param x a numeric vector
#' @param sketch the number of values to randomly sample from x, only if
#'   length(x) > sketch
#' @param \dots additional arguments passed to \code{\link[stats]{density}}
#' 
#' @return see \code{\link[stats]{density}}
#' 
#' @author Mark Cowley, 2009-01-23
#' @export
density.sketch <- function(x, sketch=10000, ...) {
	if( length(x) > sketch )
		x <- x[sample(length(x), sketch)]
	density(x, ...)
}

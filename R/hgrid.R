#' plot a grid of horizontal lines.
#' 
#' @param \dots arguments passed to \code{\link{grid}}
#' @return none.
#' @author Mark Cowley
#' @export
#' @seealso \code{\link{grid}}
hgrid <- function(...) {
	grid(ny=NULL, nx=NA, ...)
}

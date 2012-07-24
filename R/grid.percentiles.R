#' Add a grid, with lines along the percentiles or quantiles
#'
#' @inheritParams axis.percentiles
#' @inheritParams graphics::grid
#' @param \dots arguments passed to abline and grid.
#' 
#' @return nothing.
#'
#' @author Mark Cowley, 2012-07-23
#' @export
#' @rdname grid.percentiles
#' @seealso \code{\link{axis.percentiles}}
#' 
#' @examples
#' \dontrun{
#' plot(1:100, xaxt="n")
#' axis.percentiles(side=1, max=100)
#' grid.percentiles(side=1, max=100)
#' }
grid.percentiles <- function(side=1, max, col = "lightgray", lty = "dotted", lwd = par("lwd"), ...) {
	.grid.quarpertile(side, max, col, lty, lwd=lwd, N=11, ...)
}

#' @export
#' @rdname grid.percentiles
grid.quartiles <- function(side=1, max, col = "lightgray", lty = "dotted", lwd = par("lwd"), ...) {
	.grid.quarpertile(side, max, col, lty, lwd=lwd, N=5, ...)
}

#' @param N the number of lines. 5 for quartiles, 11 for percentiles
#' @noRd
.grid.quarpertile <- function(side=1, max, col = "lightgray", lty = "dotted", lwd = par("lwd"), N=c(5,11), ...) {
	tmp <- seq(0,1,length.out=N) * max
	if(side %in% c(1,3)) {
		abline(v=tmp, col=col, lty=lty, lwd=lwd, ...)
		grid(nx=0, ny=NULL, col=col, lty=lty, lwd=lwd, ...)
	}
	else {
		abline(h=tmp, col=col, lty=lty, lwd=lwd, ...)
		grid(nx=NULL, ny=0, col=col, lty=lty, lwd=lwd, ...)
	}
}

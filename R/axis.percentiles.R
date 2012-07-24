#' axis.percentiles or axis.quartiles
#'
#' @param side which side to add the axis?
#' @param max the maximum value. can't be missing
#' @return none. adds an axis to the current plot.
#' 
#' @author Mark Cowley, 2012-07-06
#' @export
#' @rdname axis.percentiles
#' @seealso \code{\link{grid.percentiles}}
#' 
#' @examples
#' \dontrun{
#' plot(1:100, xaxt="n")
#' axis.percentiles(side=1, max=100)
#' grid.percentiles(side=1, max=100)
#' }
axis.percentiles <- function(side=4, max) {
	tmp <- seq(0,1,length.out=11)
	axis(side=side, at=tmp*max, labels=paste(tmp*100, "%"))
}

#' @export
#' @rdname axis.percentiles
axis.quartiles <- function(side=4, max) {
	tmp <- seq(0,1,length.out=5)
	axis(side=side, at=tmp*max, labels=paste(tmp*100, "%"))
}


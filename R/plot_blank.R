#' Make a blank plot.
#' 
#' This is useful if you have an automate plotting pipeline that occasionally
#' can't make the intended plot
#' but you still want the place where the plot is supposed to go to be filled.
#' 
#' @param main you can optionally write a main title, and a sub-title
#' @param sub you can optionally write a main title, and a sub-title
#' @param message message to write in the middle of the plotting device
#' @param box logical: draw a box around the plot?
#' @author Mark Cowley, long time ago!
#' @export
plot_blank <- function(main=NULL, sub=NULL, message=NULL, box=FALSE) {
	plot(0, type="n", ann=FALSE, xaxt="n", yaxt="n", bty="n", xlim=c(0,1), ylim=c(0,1))

	title(main=main, sub=sub)

	if( !is.null(message) ) {
		text(0.5,0.5, message, adj=0.5)
	}
	
	if( box ) {
		box()
	}
}

#' Function to tell you where the middle of each bar is in a barplot.
#' 
#' @param height see \code{\link{barplot}}
#' @param width see \code{\link{barplot}}
#' @param space see \code{\link{barplot}}
#' @param beside see \code{\link{barplot}}
#' @param horiz see \code{\link{barplot}}
#' @param \dots see \code{\link{barplot}}
#' @return a vector of midpoints of each bar in a barplot
#' @author Mark Cowley, 8/1/07
#' @export
barplot.getx <- function(height, width = 1, space = NULL, beside = FALSE,
						 horiz = FALSE, ...) {
	if( horiz )
		stop("horiz can't be TRUE")
	else if( !is.matrix(height) && is.numeric(height) ) { # height is a vector.
		if( is.null(space) ) space <- 0.2
		space <- space * width # space is a fraction of the width.

		# in the middle of the first bar...
		xpos <- (1:length(height))*width + width/2 - width
		# account for the inter-group space
		xpos <- xpos + (1:length(height)) * space
		# DONE!
		return( xpos )
	}
	else if( is.matrix(height) && !beside ) { # 1 row per 'group'; otherwise the same as a vector.
		return( barplot.getx(height[1,], width=width, space=space, beside=beside, horiz=horiz, ...) )
	}
	else if( is.matrix(height) && beside ) { ### Doesn't work if space[2] != 1
		if( is.null(space) ) space <- c(0,1)
		space <- space * width # space is a fraction of the width.
		
		if(space[2] != 1) warning("Code doesn't work if space[2] != 1.\n")
		
		# in the middle of the first bar...
		xpos <- (1:prod(dim(height)))*width + width/2
		# account for the inter-group space
		xpos <- xpos + (rep(c(1:ncol(height)), each=nrow(height)) - 1 ) * (space[2]-space[1]) # we'll add space[1] back in later.
		# account for intra-group space
		xpos <- xpos + (1:length(xpos) -1) * space[1]
		# DONE!
		return( xpos )
	}
	else {
		stop("combination of parameters is currently unsupported")
	}
}


#' Function to determine the height of each bar with extra padding.
#' 
#' Why???
#' you might want to add text above the positive bars, and below the negative
#' bars.
#' 
#' @param height the same vector that you passed to barplot
#' @param space the gap between top/bottom of bar and the point, as a
#'   proportion of the y-range
#' @param \dots currently ignored
#' @return a numeric vector 
#' Todo: handle \code{beside=FALSE} and matrices for height
#' @author Mark Cowley, 2009-03-20
#' @export
barplot.gety <- function(height, space=0.05, ...) {
	yrange <- diff(par()$usr[3:4])
	delta <- space * yrange
	res <- ifelse(height>0, height+delta, height-delta)
	return( res )
}


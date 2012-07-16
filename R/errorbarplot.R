#' barplot with errorbars
#' 
#' 'height' and 'errors' much match exactly in dimension, and
#' can either be both vectors, or both matrices.
#' 
#' @param height see \code{\link{barplot}}
#' @param width see \code{\link{barplot}}
#' @param space see \code{\link{barplot}}
#' @param beside see \code{\link{barplot}}
#' @param horiz see \code{\link{barplot}}
#' @param ylim see \code{\link{barplot}}
#' @param xlim see \code{\link{barplot}}
#' @param \dots see \code{\link{barplot}}
#' @param errors the object containing errors (eg standard deviation) with the
#'   SAME size and dimension as \code{height}.
#' @param errors.col the colour of the errors bars
#' @param errors.width the proportion of width that the ticks should be at the
#'   top/bottom or lhs/rhs of the error tick
#' @param errors.type plot the errors bars going into and outof the bar itself
#'   (eg just "+" will plot the tick pointing out of the bar)
#' @param pvalue.stars stars to add above significant bars, or \code{NULL}. 
#'    must be same dimensions as \code{height} and \code{errors}
#' @param pvalue.stars.pos where to put the stars? one of above, top, right, 
#'    below, bottom, left
#' @param pvalue.stars.cex size of the stars
#' @param pvalue.stars.col colour of the stars
#' @param pvalue.stars.space buffer between stars and the errorbar
#' @author Mark Cowley, 28 April 2006
#' @export
errorbarplot <- function( height, width=1, space=NULL, beside=FALSE, horiz=FALSE, ylim=NULL, xlim=NULL,	 ...,
							errors=NULL, errors.col=par("fg"), errors.width=0.15, 
							errors.type=c("+", "-"), 
							pvalue.stars=NULL,
							pvalue.stars.pos=c("none", "above", "below", "top", "bottom", "left", "right"),
							pvalue.stars.cex=1, pvalue.stars.col="black", pvalue.stars.space=0.05 ) {
	pvalue.stars.pos <- pvalue.stars.pos[1]
	
	if( is.null(errors) || all(is.na(errors)) ) {
		barplot( height=height, width=width, space=space, beside=TRUE, horiz=horiz, ... )
		invisible(TRUE)
	}
	#
	# If you're adding errors bars, you will also need to increase the
	# ylim|xlim depending on the direction of the bars.
	#
	if( horiz == FALSE && is.null(ylim) ) {
		ylim <- c(min(height, 0), max(height, 0))
		if( "-" %in% errors.type )
			ylim[1] <- min(height-errors, 0)
		if( "+" %in% errors.type )
			ylim[2] <- max(height+errors, 0)
		if( pvalue.stars.pos %in% c("above", "top", "right"))
			ylim[2] <- ylim[2] + dRange(ylim)*pvalue.stars.space
		if( pvalue.stars.pos %in% c("below", "bottom", "left"))
			ylim[1] <- ylim[1] - dRange(ylim)*pvalue.stars.space
		# cat(ylim, "\n")
	}
	if( horiz == TRUE && is.null(xlim) ) {
		xlim <- c(min(height, 0), max(height, 0))
		if( "-" %in% errors.type )
			xlim[1] <- min(height-errors, 0)
		if( "+" %in% errors.type )
			xlim[2] <- max(height+errors, 0)
		if( pvalue.stars.pos %in% c("above", "top", "right"))
			xlim[2] <- xlim[2] + dRange(xlim)*pvalue.stars.space
		if( pvalue.stars.pos %in% c("below", "bottom", "left"))
			xlim[1] <- xlim[1] - dRange(xlim)*pvalue.stars.space
		# cat(xlim, "\n")
	}

	#
	# barplot as normal
	# (remember the bar midpoints)
	mp <- barplot( height=height, width=width, space=space, beside=TRUE, horiz=horiz, ylim=ylim, xlim=xlim, ... )

	#
	# add the error bars.
	#
	add.errorbars( height=height, width=width, space=space, beside=TRUE, horiz=horiz,
				   errors=errors, errors.col=errors.col, errors.width=errors.width,
				   errors.type=errors.type )
	
	if( pvalue.stars.pos != "none" ) {
		if( !horiz ) {
			xpos <- colMeans(mp)
			# xpos <- barplot.getx(height, width=width, space=space, beside=beside, horiz=horiz)
			# ypos <- height + errors
			maxY <- apply(height+errors, 2, max)
			minY <- apply(height-errors, 2, min)
			
			ypos <- switch(
				pvalue.stars.pos,
				above = maxY,
				top = rep(max(c(maxY, 0)), length(xpos)),
				below = minY,
				bottom = rep(min(c(minY, 0)), length(xpos))
			)
			ypos <- ypos + strheight("*")
			text(xpos, ypos, pvalue.stars, cex=pvalue.stars.cex, col=pvalue.stars.col)
		}
	}
	
	invisible(mp)
}

#' Add errorbars to a barplot.
#' 
#' Highly recommend you use \code{\link{errorbarplot}} instead, since
#' you need to provide the same arguments as for \code{\link{errorbarplot}}
#'  that affect the bar
#' size and spacing to this function since the placing of the error bars
#' in the middle of the bars is impossible to achieve otherwise.
#' 
#' @param height see \code{\link{errorbarplot}}
#' @param width see \code{\link{errorbarplot}}
#' @param space see \code{\link{errorbarplot}}
#' @param beside see \code{\link{errorbarplot}}
#' @param horiz see \code{\link{errorbarplot}}
#' @param \dots see \code{\link{errorbarplot}}
#' @param errors the object containing errors (eg standard deviation) with the
#'   SAME size/dimension as height.
#' @param errors.col the colour of the errors bars
#' @param errors.width the proportion of width that the ticks should be at the
#'   top/bottom or lhs/rhs of the error tick
#' @param errors.type plot the errors bars going into and outof the bar itself
#'   (eg just "+" will plot the tick pointing out of the bar)
#' @param prespace mostly used for interal processing when height and errors is
#'   a \code{matrix}.
#' @author Mark Cowley, 28 April 2006
#' @export
add.errorbars <- function( height, width=1, space=NULL, beside=FALSE, horiz=FALSE,
						   errors=NULL, errors.col=par("fg"), errors.width=0.15, errors.type=c("+", "-"),
						   prespace=0 ) {

	if( is.matrix.like(height) && beside == FALSE ) {
		cat( "can't add error bars when 'beside' is FALSE\n" )
	}
	else if( is.matrix.like(height) && beside == TRUE ) {
		#
		# assumption: width is at most nrow in length
		#
		if( is.null(space) )
			space <- c(0, 1) # within group space, between group space where a group is a row
		else if( length(space) == 1 )
			space <- rep(space, 2)
		else if( length(space) > 2 )
			space <- space[1:2]

		#
		# assumption: width is at most 2 in length
		#
		if( length(width) < nrow(height) )
			width <- recycle(width, nrow(height))
		else if( length(width) > nrow(height) )
			width <- width[1:nrow(height)]

		#
		# If width != 1.0 then the spaces need to change accordingly.
		#
		space <- space * width[1]

		#
		# What is the size of a group from the y-axis [x-axis], to the RHS [top]
		# of the last bar in the first group when horiz == FALSE [TRUE]?
		#
		# for first group of N bars (from N rows of col one):
		#
		# axis; then
		# space[2]; then
		# then bar1 with width[1] then a space[1]; then
		# then bar2 with width[2] then a space[1]; then
		# ...
		# then barN with width[N] then NO space[1]
		#
		group.width <- space[2] + sum(width) + space[1]*(nrow(height) - 1)

		#
		# For each group of bars (ie rows in height)...
		#
		for(j in 1:ncol(height)) {
			prespace <- (j-1)*group.width + space[2] - space[1]
			add.errorbars( height=height[,j], width=width, space=space[1], beside=beside, horiz=horiz,
						   errors=errors[,j], errors.col=errors.col, errors.width=errors.width, errors.type=errors.type,
						   prespace=prespace )
		}
	}
	else if( is.vector(height) && is.vector(errors) ) {
		if( length(width) < length(height) )
			width <- recycle(width, length(height))
		if( length(errors.col) < length(height) )
			errors.col <- recycle(errors.col, length(height))

		if( is.null(space) )
			space <- 0.2
		#
		# since space is a FRACTION of the average bar width left before each bar...
		# need to multiply by the width -- no change obviously if width == 1
		#
		space <- space * width[1]

		#
		# where are the centres of each of the bars given 'space' and 'width'
		#
		bar.centres <- prespace + c(1:length(height)) * (width + space) - 0.5*width

		#
		# what are the extents of the errors bars?
		#
		# User can specify whether to draw the error bars above and or
		# below the bar via errors.type = c("+", "-")
		#
		if( "-" %in% errors.type ) from <- height - errors
		else from <- height
		if( "+" %in% errors.type ) to <- height + errors
		else to <- height

		#
		# draw each error bar one at a time
		#
		for( i in 1:length(from) ) {
			#
			# draw an 'I' shaped tick if horiz == FALSE OR
			# draw and 'H' shaped tick if horiz == TRUE
			#
			if( !horiz ) { # vertical bars, so top to bottom ticks
				lines( rep(bar.centres[i], 2),
						c(from[i], to[i]),
						col=errors.col[i] )
				#
				# Draw the top and bottom of 'I' shaped tick
				#
				if("-" %in% errors.type)
					lines( c(bar.centres[i] - errors.width*width[i], bar.centres[i] + errors.width*width[i]),
							rep(from[i], 2), col=errors.col[i] )
				if("+" %in% errors.type)
					lines( c(bar.centres[i] - errors.width*width[i], bar.centres[i] + errors.width*width[i]),
							rep(to[i], 2), col=errors.col[i] )
			}
			else { # horizontal bars, so left to right ticks
				lines( c(from[i], to[i]),
						rep(bar.centres[i], 2),
						col=errors.col[i] )
				#
				# Draw the left and right of 'H' shaped tick
				#
				if("-" %in% errors.type)
					lines( rep(from[i], 2),
							c(bar.centres[i] - errors.width*width[i], bar.centres[i] + errors.width*width[i]),
							col=errors.col[i] )
				if("+" %in% errors.type)
					lines( rep(to[i], 2),
							c(bar.centres[i] - errors.width*width[i], bar.centres[i] + errors.width*width[i]),
							col=errors.col[i] )
			}
		}
	}
}


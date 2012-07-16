#' image plot of a table
#' 
#' Function to plot a \code{matrix} or \code{data.frame} as an image such that cell [1,1] is
#' in top left of the image, and cell \code{[nrow, ncol]} is in bottom right corner,
#' ie in the same orientation as \code{print} or \code{write.table} would show it.
#' Note that \code{\link{image}} rotates the image along the diagonal.
#' 
#' @param x a \code{matrix} (or \code{data.frame}) of numbers
#' @param legend add a color legend to RHS of plot
#' @inheritParams graphics::image
#' 
#' @return an image with x[1,1] in top left, and x[nrow(x), ncol(x)] in bottom
#'   left corner Todo: get legend=TRUE to work when layout or mfrow/mfcol has
#'   prev. been set.
#' @author Mark Cowley, 2 June 2005
#' 
#' @export
#' @importFrom gplots redblue
#' @importFrom graphics image
#' @import mjcbase
image.table <- function(x, main="Heatmap", 
	col=NULL, zlim=NULL,
	legend=TRUE, unlog.legend=FALSE, legend.pos="bottom", legend.main="",
	na.rm=0, 
	xlab="column index", xlabels=NULL, xlabels.pos="bottom", 
	ylab="row index", ylabels=NULL, ylabels.pos="left", 
	row.descriptions=NULL,
	grid.colour="white", ...) {

	op <- par(no.readonly = TRUE)
	if( !is.null(main) )
		par(oma=c(0,0,3,0))
		
	if( is.null(col) ) {
		min <- min(x, na.rm=TRUE)
		max <- max(x, na.rm=TRUE)

		if( min < 0 && max > 0 ) {
			# Then the data in x is roughly symmetrical, and should have a colour
			# that approximates a scale from red to blue, via white
			# if( !require( gplots, quietly=TRUE ) )
			# 	col <- c(colour.step("red", "white", steps=100), colour.step("white", "blue", steps=100)[2:100])
			# else
			# 	col <- redblue(201)
			col <- redblue(201)
			if( is.null(zlim) )
				zlim <- symmetricise(c(min,max))
		}
		else {
			# then the data is not symmetrical about zero, so the colours should rise up to
			# or start at white depending on where the min value is.
			if( min >= 0 ) {
				# then the data starts at zero and becomes positive
				col <- colour.step("white", to="blue", steps=100)
			}
			else {
				# then the data rises from negative up to slightly negative
				col <- colour.step("red", to="white", steps=100)
			}
		}
	}

	if( is.null(zlim) )
		zlim <- range(x, na.rm=TRUE)
	else {
		if( any(x>zlim[2], na.rm=TRUE) )
			x[x>zlim[2]] <- zlim[2]
		if( any(x<zlim[1], na.rm=TRUE) )
			x[x<zlim[1]] <- zlim[1]
	}

	if( legend && all( par("mfrow") == c(1,1) ) ) {
		if( legend.pos == "right" )
			layout( matrix(c(1,2), 1), widths=c(6,1) )
		else if( legend.pos == "left" )
			layout( matrix(c(2,1), 1), widths=c(1,6) )
		else if( legend.pos == "bottom" && is.null(row.descriptions) )
			layout( matrix(c(1,2), ncol=1), heights=c(15,1) )
		else if( legend.pos == "bottom" && !is.null(row.descriptions) )
			layout( matrix(c(1,2,3,0), ncol=2, byrow=FALSE), heights=c(15,1), widths=c(1,3) )
		on.exit( par(op) )
	}


	if( !is.null(na.rm) )
		x[is.na(x)] <- na.rm

	x2 <- t( x[rev(1:nrow(x)),] ) ## orient x so that image will draw it in the right orientation
	image(x2 , xaxt="n", yaxt="n", main="", xlab=xlab, ylab=ylab, col=col, zlim=zlim, ...)

	# x axis labels
	if( is.null(xlabels) ) {
		if( ncol(x) <= 25 ) {
			axis(1, at=seq(0, 1, 1/(ncol(x)-1)), tick=TRUE, labels=c(1:ncol(x)))
			axis(3, at=seq(0, 1, 1/(ncol(x)-1)), tick=TRUE, labels=FALSE)
		}
		else {
			axis(1, at=seq(0, 1, length.out=6), labels=round(seq(1, ncol(x), length.out=6)))
		}
	}
	else {
		stopifnot( ncol(x) == length(xlabels) )
		axis(ifelse(xlabels.pos=="bottom", 1, 3), at=seq(0, 1, length.out=ncol(x)), labels=xlabels, tick=FALSE, line=-0.5)
	}
	
	# y axis labels
	if( is.null(ylabels) ) {
		if( nrow(x) <= 25 ) {
			axis(2, at=seq(1, 0, length.out=nrow(x)), tick=TRUE, labels=c(1:nrow(x)))
			axis(4, at=seq(1, 0, length.out=nrow(x)), tick=TRUE, labels=FALSE)
		}
		else {
			axis(2, at=seq(1, 0, length.out=6), labels=round(seq(1, nrow(x), length.out=6)))
		}
	}
	else {
		stopifnot( nrow(x) == length(ylabels) )
		axis(side=ifelse(ylabels.pos=="left", 2, 4), at=seq(1, 0, length.out=nrow(x)), labels=ylabels, tick=FALSE, line=-0.5, font=3)
	}
	box()
	
	mtext(side=3, outer=TRUE, main, las=1)
	
	#
	# add a grid???
	#
	if( !is.null(grid.colour) ) {
		v <- seq(0, 1, 1/(ncol(x)-1))
		v <- v[1:(length(v)-1)]
		v <- v + (v[2]-v[1])/2
		abline(v=v, col=grid.colour)

		h <- seq(1, 0, length.out=nrow(x))
		h <- h[1:(length(h)-1)]
		h <- h + (h[2]-h[1])/2
		abline(h=h, col=grid.colour)
		
	}

	# remember the current par usr in case we need to add in row descriptions later on.
	usr <- par()$usr
	mids <- seq(1,0, length.out=nrow(x))

	# add a colour bar?
	if( legend ) {
		z <- seq(zlim[1], zlim[2], length=length(col))
		z <- matrix(z, nrow=1)

		marB4 <- par("mar")
		

		if( legend.pos %in% c("left", "right") ) {
			mar <- c(8,2,8,0.5)
		}
		else if( legend.pos == "bottom" ) {
			# mar <- c(1,4,2,4)
			mar <- c(1,marB4[2]+2,2,marB4[4]+2)
			z <- t(z)
		}
		# yrange <- diff(mar[c(1,3)])
		# mar[2] <- 1 + unlog.legend
		# mar[1] <- mar[1] + 2
		# mar[3] <- mar[3] + 2
		# mar[4] <- 1
		# if( legend.pos == "left" ) {
		# 	mar[c(2,4)] <- mar[c(4,2)]
		# }
		par(mar=mar)

		image(z, col=col, xaxt="n", yaxt="n", main=legend.main)
		box()
		lab <- seq(zlim[1], zlim[2], length=5)
		if( unlog.legend )
			lab <- round(10^lab, 2)
		else
			lab <- round(lab, 2)
		if( legend.pos %in% c("left", "right") ) {
			axis(2, at=seq(0, 1, length=5), labels=lab, 2)
		}
		else if( legend.pos %in% c("bottom") ) {
			axis(3, at=seq(0, 1, length=5), labels=lab, 2, las=1)
		}
		
		par(mar=marB4)
##		 mtext("legend\n")
	}

	if( !is.null(row.descriptions) ) {
		plot.new()
		par(usr=c(0,1,usr[3:4]))
		par(mar=c(marB4[1],0,marB4[3],0))
		adj <- 0.03
		x.pos <- rep(-adj, length(row.descriptions))
		# y.pos <- seq(1+adj, 0-adj, length.out=nrow(x))
		text(x.pos, mids, row.descriptions, adj=c(0,0.5))
	}
##	 par(op)
}

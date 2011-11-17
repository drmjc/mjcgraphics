## Function to label

text.stack <- function(x, y, labels, col=NULL, X=NULL, buffer=0.01, sort=F, ...) {
	xrange <- par("usr")[2] - par("usr")[1]
	yrange <- ( par("usr")[4] - par("usr")[3] )
## print(xrange)
## print(yrange)
	buffer <- xrange * buffer
## print(buffer)
	if( is.null(X) )
		## Assume X should be on lhs, 5% from edge.
		X <- par("usr")[1] + max(strwidth(labels)) * 1.1

	if( length(X) == 1 )
		X <- rep(X, length(x))

	if( is.null(col) )
		col <- rep(1, length(labels))


	cex.old <- par("cex")
	## Check that the labels will all fit with no overlap
	ychar <- strheight(labels[1]) * 1.2 ## allow some gap
## print(strheight(labels[1]))
## print(ychar*length(labels))
## print(yrange*0.9)
## print(par("cex"))
	if ( ychar*length(labels) > yrange*0.9 ) {
		## cat("Adjusting cex to fit the labels\n")
		## Need to adjust the cex to fit the labels in.
		par( cex=par("cex")*(yrange*0.9/(ychar*length(labels))) )
	}
## print(par("cex"))

	Y <- seq(par("usr")[3] + yrange*0.05,  par("usr")[4] - yrange*0.05, by=yrange*0.9/(length(labels)-1))[rank(y)]
## print(cor(y,Y))
	if(sort) {
		order <- order(y, decreasing=T)
		X <- X[order]
		Y <- Y[order]
		x <- x[order]
		y <- y[order]
		labels <- labels[order]
		col <- col[order]
	}
	text(X, Y, labels, col=col, pos=2, offset=0.2, ...)

	for(i in 1:length(x)) {
		segments(X[i], Y[i], x[i] - buffer, y[i], col=col[i])
	}

	par(cex=cex.old)
	invisible(cbind(x=x,y=y,X=X,Y=Y,labels=labels))
}

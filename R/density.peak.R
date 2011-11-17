## Find the elemnt at the peak of the density distribution
##
## Mark Cowley, 12 July 2005
##
density.peak <- function(data) {
	if( !is.null(ncol(data)) && (ncol(data) > 1) ) {
		res <- NULL
		for( i in 1:ncol(data) )
			res <- rbind(res, density.peak(data[,i]) )
		if(!is.null(colnames(data)))
			rownames(res) <- colnames(data)
		return( as.data.frame( res ) )
	}
	else {
		tmp <- density(data)
		idx <- which.max(tmp$y)
		return( c(x=tmp$x[idx], y=tmp$y[idx]) )
	}
}

get.density.peaks <- function(data) {
	density.peak(data)
}

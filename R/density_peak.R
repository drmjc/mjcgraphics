#' Find the element at the peak of the density distribution
#' 
#' @param data a matrix or vector of data
#' @author Mark Cowley, 12 July 2005
#' @export
#' @rdname density_peak
density_peak <- function(data) {
	if( !is.null(ncol(data)) && (ncol(data) > 1) ) {
		res <- NULL
		for( i in 1:ncol(data) )
			res <- rbind(res, density_peak(data[,i]) )
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

#' @export
#' @rdname density_peak
get_density_peaks <- density_peak

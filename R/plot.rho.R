#' rank-based correlation heatmap
#' 
#' Work out the correlation of N samples using Spearmans rho, then
#' plot these correlations as a heatmap, and return the average correlation of
#' each sample
#' 
#' @param x A \code{matrix} or \code{data.frame} of data, with samples in column
#' (eg gene expression data), \emph{or} a pre-computed correlation matrix (which must be
#' square with a diagonal of 1's)
#' @param name Used for naming the plots appropriately
#' @param do.mfrow logical: \code{TRUE} means call \code{par(mfrow=c(1, 2))}; 
#' \code{FALSE} means don't set the graphical layout
#' @param col the colour to use. It will be recycled if necessary
#' 
#' @return invisibly returns the average correlation of each column
#' 
#' @author Mark Cowley, 9 November 2005
#' @export
plot.rho <- function(x, do.mfrow=TRUE, name=paste(ncol(x), "arrays"), col=1) {
    ## does col need recycling?
    col <- recycle( col )

    ## is x the expression data or a correlation matrix
    if( nrow(x) == ncol(x) && all(diag(x) == 1) )
        rho <- x
    else
        rho <- rho(x)

    rho.av <- rowMeans(rho)

    if( do.mfrow )
        par(mfrow=c(1,2))

    image.table(rho, main=paste("rho correlations of ", name, sep=""), xlab="array index", ylab="array index")
    plot(rho.av, main=paste("average rho correlations of ", name, sep=""), ylab="rho", xlab="array index")

    invisible( rho.av )
}

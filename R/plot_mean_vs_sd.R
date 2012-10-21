#' plot mean vs stdev
#' 
#' Plot the means of the rows of x (on x-axis) vs the sd of the rows of x (on
#' y-axis)
#' 
#' @param x a \code{matrix} or numeric \code{data.frame}
#' @param xlab see par
#' @param ylab see par
#' @param main see par
#' @param \dots arguments passed to \code{plot}
#' @author Mark Cowley, 3 April 2006
#' @export
plot_mean_vs_sd <- function(x, xlab=NULL, ylab=NULL, main="Per-ProbeSet mean vs sd", ...) {
    means <- rowMeans(x, na.rm=TRUE)
    sd <- rowSD(x, na.rm=TRUE)
    nas <- union(which(is.na(means)), which(is.na(sd)))
    if( length(nas) > 0 ) {
        means <- means[-nas]
        sd <- sd[-nas]
    }

    plot(means, sd, xlab="", ylab="", main=main, type="p", ...)
    if( is.null(xlab) )
        mtext(side=1, adj=0.5, text=expression(bar(X)), outer=FALSE, line=2)
    else
        mtext(side=1, adj=0.5, text=xlab, outer=FALSE, line=2)

    if( is.null(ylab) )
        mtext(side=2, adj=0.5, text=expression(sigma), outer=FALSE, line=3)
    else
        mtext(side=2, adj=0.5, text=ylab, outer=FALSE, line=3)

    lines(lowess(means, sd, f=1/5), col=6)

}

#' plot mean vs variance
#' 
#' Plot the means of the rows of x (on x-axis) vs the variance of the rows of x
#' (on y-axis)
#' 
#' @param x a \code{matrix} or numeric \code{data.frame}
#' @param xlab see par
#' @param ylab see par
#' @param main see par
#' @param \dots arguments passed to \code{plot}
#' @author Mark Cowley, 3 April 2006
#' @export
plot_mean_vs_var <- function(x, xlab=NULL, ylab=NULL, main="Per-ProbeSet mean vs var", ...) {
    means <- rowMeans(x, na.rm=TRUE)
    var <- rowVar(x, na.rm=TRUE)
    nas <- union(which(is.na(means)), which(is.na(sd)))
    if( length(nas) > 0 ) {
        means <- means[-nas]
        var <- var[-nas]
    }

    plot2(means, var, xlab="", ylab="", main=main, type="p", ...)
    if( is.null(xlab) )
        mtext(side=1, adj=0.5, text=expression(bar(X)), outer=FALSE, line=2)
    else
        mtext(side=1, adj=0.5, text=xlab, outer=FALSE, line=2)

    if( is.null(ylab) )
        mtext(side=2, adj=0.5, text=expression(sigma^2), outer=FALSE, line=3)
    else
        mtext(side=2, adj=0.5, text=ylab, outer=FALSE, line=3)

    lines(lowess(means, var, f=1/5), col=6)

}

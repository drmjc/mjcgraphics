## When you call abline, it draws a line all the way to the extremes of the
## plotting region which can look ugly, especially on vertical ablines when
## the y==0 axis is some way away from the labelled axis.
##
## Use abline0 to draw horizontal or vertical ablines that start (or stop)
## at zero.
##
## NB, untf is not supported, nor are any mean of calling abline other than
## by setting h=_ or v=_
##
## Mark Cowley, 26 March 2006
##
abline0 <- function(h = NULL, v = NULL, untf=F, ...) {
    if( untf )
        stop("untf not supported -- use abline\n")

    if( is.null(h) && is.null(v) ) {
        warning("currently only h and v are supported\n")
        abline(...)
    }
    else if( !is.null(h) ) {
        if( 0 < median(par("usr")[1:2]))
            lines( c(0, par("usr")[2]), rep(h,2), ... )
        else
            lines( c(par("usr")[1], 0), rep(h,2), ... )
    }
    else if( !is.null(v) ) {
        if( 0 < median(par("usr")[3:4]))
            lines( rep(v, 2), c(0, par("usr")[4]), ... )
        else
            lines( rep(v, 2), c(par("usr")[3], 0), ... )
    }
}

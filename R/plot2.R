#' plot2
#' 
#' A versatile plotting function which allows the user to: - change the
#' background colour of the plotting region - add ticks to all 4 axes - change
#' the labels for the x and y axes in one command - point the axis ticks
#' inwards (make tcl positive)
#' 
#' NB: axes are added after plotting the data using axis and title commands.
#' 
#' @param x see ?par or ?plot.
#' @param y see ?par or ?plot.
#' @param main see ?par or ?plot.
#' @param sub see ?par or ?plot.
#' @param xlab see ?par or ?plot.
#' @param ylab see ?par or ?plot.
#' @param xlim see ?par or ?plot.
#' @param ylim see ?par or ?plot.
#' @param col see ?par or ?plot.
#' @param lty see ?par or ?plot.
#' @param lwd see ?par or ?plot.
#' @param type see ?par or ?plot.
#' @param tcl commonly overlooked parameter which allows the ticks to point in
#'   (positive) or out of the plot (negative). set to NULL ro retain default
#'   behaviour.
#' @param bg.col change the background colour. Set to NULL to retain default
#'   behaviour
#' @param ticks which axes should have tick marks plotted?
#' @param xlabels override the automatic labels added to x-axis. NB it's up to
#'   the user to supply either the same number of labels as normally computed
#'   by axTicks(side=1), or if more labels than spots for labels are supplied,
#'  then they are placed at \code{1:length(xlabels)}.
#' @param ylabels as above for side=2.
#' 
#' @return none. makes a plot
#' 
#' @author Mark Cowley, 28 March 2006
#' @export
plot2 <- function(x, y=NULL, bg.col="lightgrey",
                  main=NULL, sub=NULL,
                  ticks=c(1,2,3,4),
                  xlab="", xlim=NULL, xlabels=NULL,
                  ylab="", ylim=NULL, ylabels=NULL,
                  vablines=NULL, vabline.col="darkgrey", vabline.lty=1,
                  hablines=NULL, habline.col="darkgrey", habline.lty=1,
                  col=1, lty=1, lwd=1, type="l", axes=TRUE, tcl=0.25, ...) {

    if( is.list(x) )
        x <- as.numeric(x)
    if( is.list(y) )
        y <- as.numeric(y)

    #
    # intially, don't plot the points, just set up the plotting area
    #
    plot(x=x, y=y, main=main, sub=sub, xlab="", ylab="", ylim=ylim, xlim=xlim,
##          col=col, lty=lty, lwd=lwd,
         type="n", axes=FALSE)

    #
    # Draw the bg colour if it's been set
    #
    if( !is.null(bg.col) ) {
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col=bg.col)
    }

    #
    # Draw the ablines before the points -- this looks better for publication
    # quality plots.
    #
    if( !is.null(vablines) ) {
        abline(v=vablines, col=vabline.col, lty=vabline.lty)
    }
    if( !is.null(hablines) ) {
        abline(h=hablines, col=habline.col, lty=habline.lty)
    }

    #
    # draw the points on top of the bg rectangle
    #
    points(x, y, col=col, lty=lty, lwd=lwd, type=type, ...)

    #
    # set up the axes
    #
    if( axes ) {
        box()

        #
        # set up the x axis
        #
        xat <- axTicks(side=1)
        if( !is.null(xlabels) && (length(xlabels) != length(xat)) )
            xat <- 1:length(xlabels)
        if(1 %in% ticks) axis(side=1, tcl=tcl, tick=TRUE, at=xat, labels=xlabels)
        if(3 %in% ticks) axis(side=3, tcl=tcl, tick=TRUE, at=xat, labels=FALSE)

        #
        # set up the y axis
        #
        yat <- axTicks(side=2)
        if( !is.null(ylabels) && (length(ylabels) != length(yat)) )
            yat <- 1:length(ylabels)
        if(2 %in% ticks) axis(side=2, tcl=tcl, tick=TRUE, at=yat, labels=ylabels )
        if(4 %in% ticks) axis(side=4, tcl=tcl, tick=TRUE, at=yat, labels=FALSE )
    }

    #
    # add the axis labels
    #
    if( is.null(xlab) || (!is.expression(xlab) && xlab == "") )
        dummy <- TRUE
    else
        title( xlab=xlab )

    if( is.null(ylab) || (!is.expression(ylab) && ylab == "") )
        dummy <- TRUE
    else
        title( ylab=ylab )

    ##     if( !is.null(xlab) | xlab != "") mtext(xlab, side=1, line=-3, outer=TRUE, at=0.5)
    ##     if( !is.null(ylab) | ylab != "") mtext(ylab, side=2, line=-3, outer=TRUE, at=0.5)

    #
    #
    #
}

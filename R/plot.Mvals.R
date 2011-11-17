## Function to plot the Mvalues from multiple genes in a cohort.
##
##  see plot.matrix or plot2 for description of extra arguments.
##
## Mark Cowley, 5 April 2006
##
plot.Mvals <- function( expression,
                        bg.col=NULL, main=NULL, sub=NULL,
                        ticks=c(1,2,3,4),
                        xlab="strain", xlim=c(1, ncol(expression)), xlabels=colnames(expression),
                        ylab="Expression Ratio", ylim=NULL, ylabels=NULL,
                        vablines=NULL, vabline.col="lightgrey", vabline.lty=1,
                        hablines=0, habline.col="darkgrey", habline.lty=2,
                        col=1, lty=1, lwd=1, pch=1, type="l", axes=T, tcl=0.30,
                        add=F,
                        meancorrect=F, symmetrical=T, lty.cor=T ) {


        if( is.null(main) ) {
            main <- paste(nrow(expression), "genes")
        }

        if( meancorrect ) {
            expression <- expression - rowMeans(expression)
        }

        if( is.null(ylim) )
            ylim <- range(expression, na.rm=T)

        las.b4 <- par("las")
        on.exit(par(las.b4))

        par(las=2) # horizontal labels
##         par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5)

        #
        #
        #
        if(add)
            plot.matrix( expression, add=T, col=col, lty=lty, lwd=lwd, pch=pch )
        else {
            #
            # wow, this is getting complicated...
            #
#             par(cex=0.75) # MJC: 30/5/07
            plot.matrix( expression, bg.col=bg.col,
                         main=main, sub=sub,
                         ticks=ticks,
                         xlab="", xlim=xlim, xlabels=xlabels,
                         ylab=ylab, ylim=ylim, ylabels=F,
                         vablines=vablines, vabline.col=vabline.col, vabline.lty=vabline.lty,
                         hablines=hablines, habline.col=habline.col, habline.lty=habline.lty,
                         col=col, lty=lty, lwd=lwd, pch=pch, type=type, axes=axes, tcl=tcl,
                         add=F, auto.log=F,
                         symmetrical=symmetrical, lty.cor=lty.cor )
            #
            # annotate the LHS axis with the M value as a ratio.
            #
            if( axes && !is.null(ylabels) && ylabels ) {
                axis( side=2, at=c(-6:6),
                    labels=c("1:64", "1:32", "1:16", "1:8", "1:4", "1:2", "1:1", "2:1", "4:1", "8:1", "16:1", "32:1", "64:1"), tcl=tcl, cex=2.5 )
            }

            if( !is.null(xlab) ) {
                title(xlab=xlab, line=par("mgp")[1]+1)
            }

        }
        par(las.b4)
}

axis.Mvals <- function(side=2, tcl=0.30, cex=2.5, ...) {
    extremes <- par()$usr

    axis( side=side, at=c(-6:6),
          labels=c("1:64", "1:32", "1:16", "1:8", "1:4", "1:2", "1:1", "2:1", "4:1", "8:1", "16:1", "32:1", "64:1"), tcl=tcl, cex=cex )

}

## Function to plot the Mvalues from multiple genes in a cohort.
##
##  see plot.matrix or plot2 for description of extra arguments.
##
## Mark Cowley, 5 April 2006
##
plot.Avals <- function( expression,
                        bg.col=NULL, main=NULL, sub=NULL,
                        ticks=c(1,2,3,4),
                        xlab="strain", xlim=c(1, ncol(expression)), xlabels=colnames(expression),
                        ylab="Expression Level (log2)", ylim=NULL, ylabels=NULL,
                        vablines=NULL, vabline.col="lightgrey", vabline.lty=1,
                        hablines=NULL, habline.col="darkgrey", habline.lty=2,
                        col=1, lty=1, lwd=1, pch=1, type="l", axes=T, tcl=0.30,
                        add=F,
                        lty.cor=T,
                        unlog.ylabels=F ) {


        if( is.null(main) ) {
            main <- paste(nrow(expression), "genes")
        }

        if( is.null(ylim) )
            ylim <- range(expression, na.rm=T)

        las.b4 <- par("las")

        par(las=2) # horizontal labels

        #
        #
        #
        if(add)
            plot.matrix( expression, add=T, col=col, lty=lty, lwd=lwd, pch=pch )
        else {
            #
            # wow, this is getting complicated...
            #
            plot.matrix( expression, bg.col=bg.col,
                         main=main, sub=sub,
                         ticks=ticks,
                         xlab="", xlim=xlim, xlabels=xlabels,
                         ylab=ylab, ylim=ylim, ylabels=F,
                         vablines=vablines, vabline.col=vabline.col, vabline.lty=vabline.lty,
                         hablines=hablines, habline.col=habline.col, habline.lty=habline.lty,
                         col=col, lty=lty, lwd=lwd, pch=pch, type=type, axes=axes, tcl=tcl,
                         add=F, auto.log=F,
                         symmetrical=F, lty.cor=lty.cor )
            #
            # annotate the LHS axis with the A value as either and unlogged
            # number, or the value on the original logged scale.
            #
            if( axes && !is.null(ylabels) && ylabels ) {
                if( unlog.ylabels )
                    axis( side=2, at=c(0:16), tick=T,
                        labels=2^c(0:16), tcl=tcl )
                else
                    axis( side=2, at=c(0:16), tick=T,
                        labels=T, tcl=tcl )
            }

            if( !is.null(xlab) ) {
                title(xlab=xlab, line=par("mgp")[1]+1)
            }
        }

        par(las.b4)
}


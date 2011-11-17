## Wrapper to plot.Avals.3tissues and plot.Mvals.3tissues.
##
## Mark Cowley, 5 April 2006
plot.MAvals.3tissues <- function( genes,
                                  expression.M=expressed755.qpt,
                                  expression.A=expressionA.6k,
                                  main=NULL, bg.col="white",
                                  hline=0, vablines=1:ncol(expression.M[[1]]),
                                  tcl=0.4, lty=1, type="l", pch=1, centroid=T, lty.cor=T ) {
    if( centroid )
        layout(matrix(c(0,1,0,2,0,3,0,4,0,
                        0,5,0,6,0,7,0,8,0), 2, byrow=T), widths=c(2,5,0.5,5,0.5,5,0.5,5,0.5))
    else
        layout(matrix(c(0,1,0,2,0,3,0,
                        0,4,0,5,0,6,0), 2, byrow=T), widths=c(1,5,0.5,5,0.5,5,0.5))

    plot.Mvals.3tissues(genes, expression=expression.M, symmetrical=T, bg.col="white", centroid=centroid, do.mfrow=F,
                        main=main, vablines=vablines, hablines=hline,
                        tcl=tcl, lty=lty, type=type, pch=pch, lty.cor=lty.cor)
    plot.Avals.3tissues(genes, expression=expression.A, bg.col="white", centroid=centroid, do.mfrow=F,
                        main=main, vablines=vablines,
                        tcl=tcl, lty=lty, type=type, pch=pch, lty.cor=lty.cor)
}











## Plot the expression RATIOS (Mvalues) for a set of genes in three tissues.
##
## Parameters:
##     genes: a character vector of gene names.
##     expression: a list of at least three expression matrix-like objects,
##             called, "B", "K", and "L"
##
##     main: The plot titles. If left as null, the number of genes plotted
##           will prefix the tissue (eg: "32 genes Brain"). Try setting to
##           main="" to create a title of just "Brain" for eg.
##
##     bg.col: The background colour for the plots
##
##     meancorrect: Should the expression measurements be mean corrected?
##
##     symmetical: make the M value plots be symmetical about M == 0?
##
##     centroid: calculate the centroid of the set of genes in each tissue
##             and plot these 3 lines in a separate (4th) plot.
##
##     hablines: draw a darggrey, dashed horizontal line, centered at a specified
##             value? Leave as NULL to not plot it.
##
##     vablines: draw lightgrey vertical lines every 2nd measurement, to assist
##             in lining up the peaks and troughs of the plotted lines.
##
##     do.mfrow: if T, then the plotting device is automatically set up into
##             the apprpriate number of sub-panels.
##
##     unlog: unlog the Mvalues? not sure why you'd want to do this
##
##     tcl: change the tick length and direction.
##
## Value:
##     none
##
## Mark Cowley, 7 March 2006
##
plot.Mvals.3tissues <- function( genes, expression=expression[1:3],
                                 main=NULL, bg.col=NULL,
                                 meancorrect=T, symmetrical=T, centroid=F,
                                 hablines=NULL,
                                 vablines=1:ncol(expression[[1]]),
                                 do.mfrow=T,
                                 unlog=F, tcl=0.2, lty=1, type="l", pch=1, lty.cor=T ) {

    if( length(genes) == 0 ) {
        # just in case there is being called recursively, and one of the elements has no genes
        for(i in 1:(3+centroid)) {
            main2 <- paste(ifelse(is.null(main), "0 genes,", main), c("Brain", "Kidney", "Liver", "average")[i])
            plot.blank(main=main2, box=T)
        }
        return(NULL)
    }

    if( is.null(main) ) {
        main <- paste(length(genes), "genes")
    }

    if( unlog ) {
        expression$B[genes, ] <- 2 ^ expression$B[genes, ]
        expression$K[genes, ] <- 2 ^ expression$K[genes, ]
        expression$L[genes, ] <- 2 ^ expression$L[genes, ]
    }

    ylim <- range(rbind(expression$B[genes, ], expression$K[genes, ], expression$L[genes, ]), na.rm=T)
    if( symmetrical )
        ylim <- symmetricise( ylim )

    ylab <- "Expression Ratio"

##         if(do.mfrow)
##             par(mfrow=c(1, 3 + centroid))
    if( do.mfrow ) {
        par.b4 <- par(no.readonly=T)
        on.exit( par(par.b4) )
        if( centroid )
            layout(matrix(c(0,1,0,2,0,3,0,40,0), 1), widths=c(2,5,0.5,5,0.5,5,0.5,5,0.5))
        else
            layout(rbind(matrix(c(0,1,0,2,0,3,0), 1), 0), widths=c(2,5,0.5,5,0.5,5,0.5), height=c(10,1))
    }

    par(cex.main=1.5)
#     par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    par(mar=c(2,0,4,0))
    par(cex=0.7)

    plot.Mvals( expression$B[genes, ], col=4, bg.col=bg.col, main=paste(main, "Brain"),
                ylabels=T,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl, symmetrical=F, meancorrect=meancorrect,
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch )
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch )

    if(par("mfrow")[1] == 2)
        mtext(ylab, line=6, outer=F, side=2, las=0, adj=0.5)
    else
        mtext(ylab, line=6, outer=F, side=2, las=0, adj=0.5)

    par(mar=c(2,0,4,0))


    #
    # K
    #
    plot.Mvals( expression$K[genes, ], col=3, bg.col=bg.col, main=paste(main, "Kidney"),
                ylabels=F,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl, symmetrical=F,
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch )
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch )

    #
    # L
    #
    plot.Mvals( expression$L[genes, ], col=2, bg.col=bg.col, main=paste(main, "Liver"),
                ylabels=F,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl, symmetrical=F,
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch )
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch )

    #
    # centroid
    #
    if( centroid ) {
        tmp <- get.centroid(lapply(expression[1:3], function(x) x[genes,]))
        #
        # tmp is a matrix with 3 rows, and 31 columns.
        #
        plot.Mvals( tmp, col=4:2, bg.col=bg.col, main=paste(main, "average"),
                    ylabels=F,
                    vablines=vablines, vabline.col="lightgrey",
                    hablines=hablines, habline.col="lightgrey",
                    tcl=tcl, symmetrical=symmetrical,
                    ylim=ylim, xlab="", ylab="", lty.cor=lty.cor)
    }
}








## Plot the expression LEVELS (Avalues) for a set of genes in three tissues.
##
## Parameters:
##     genes: a character vector of gene names.
##     expression: a list of at least three expression matrix-like objects,
##             called, "B", "K", and "L"
##
##     main: The plot titles. If left as null, the number of genes plotted
##           will prefix the tissue (eg: "32 genes Brain"). Try setting to
##           main="" to create a title of just "Brain" for eg.
##
##     bg.col: The background colour for the plots
##
##     centroid: calculate the centroid of the set of genes in each tissue
##             and plot these 3 lines in a separate (4th) plot.
##
##     hablines: draw a darggrey, dashed horizontal line, centered at a specified
##             value? Leave as NULL to not plot it.
##
##     vablines: draw lightgrey vertical lines every 2nd measurement, to assist
##             in lining up the peaks and troughs of the plotted lines.
##
##     do.mfrow: if T, then the plotting device is automatically set up into
##             the apprpriate number of sub-panels.
##
##     unlog: unlog the Avalues unto natural scale.
##
##     tcl: change the tick length and direction.
##
##     unlog.ylabels: if you leave unlog=F, then you can display the labels on
##              the y axis in unlogged form. ie data is plotted in log coords
##              but in stead of 8, 9, 10 on axis, you get 2^8, 2^9, 2^10.
##
## Value:
##     none
##
## Mark Cowley, 7 March 2006
##
plot.Avals.3tissues <- function( genes, expression=expression$Avals,
                                 main=NULL, bg.col=NULL,
                                 centroid=F,
                                 hablines=NULL,
                                 vablines=1:ncol(expression[[1]]),
                                 do.mfrow=T,
                                 unlog=F, tcl=0.2, lty=1, type="l", pch=1,
                                 unlog.ylabels=T, lty.cor=T ) {

    if( length(genes) == 0 ) {
        # just in case there is being called recursively, and one of the elements has no genes
        for(i in 1:(3+centroid)) {
            main2 <- paste(ifelse(is.null(main), "0 genes,", main), c("Brain", "Kidney", "Liver", "average")[i])
            plot.blank(main=main2, box=T)
        }
        return(NULL)
    }

    if( sum(expression$B[genes,] < 0) > 0 )
        warning("Negative A values are strange. Are you sure you didn't supply Mvalues instead?\n")

    if( is.null(main) ) {
        main <- paste(length(genes), "genes")
    }

    if( unlog ) {
        expression$B[genes, ] <- 2 ^ expression$B[genes, ]
        expression$K[genes, ] <- 2 ^ expression$K[genes, ]
        expression$L[genes, ] <- 2 ^ expression$L[genes, ]
        unlog.ylabels <- F # just incase it has been left on!
    }

    ylim <- c( min(min(expression$B[genes, ], na.rm=T), min(expression$K[genes, ], na.rm=T), min(expression$L[genes, ], na.rm=T)),
                max(max(expression$B[genes, ], na.rm=T), max(expression$K[genes, ], na.rm=T), max(expression$L[genes, ], na.rm=T)) )

    ylab <- "Expression Level (log2)"

##         if(do.mfrow)
##             par(mfrow=c(1, 3 + centroid))
    if( do.mfrow ) {
        par.b4 <- par(no.readonly=T)
        on.exit( par(par.b4) )
        if( centroid )
            layout(matrix(c(0,1,0,2,0,3,0,4,0), 1), widths=c(2,5,0.5,5,0.5,5,0.5,5,0.5))
        else
            layout(matrix(c(0,1,0,2,0,3,0,), 1), widths=c(2,5,0.5,5,0.5,5,0.5))
    }

    par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
    par(mar=c(2,0,4,0))

    plot.Avals( expression$B[genes, ], col=4, bg.col=bg.col, main=paste(main, "Brain"),
                ylabels=T,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl,
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch,
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch,
                unlog.ylabels=unlog.ylabels )

    if(par("mfrow")[1] == 1)
        mtext(ylab, line=-2, outer=T, side=2, las=0)
    else
        mtext(ylab, line=-2, outer=T, side=2, las=0, at=0.75)

    par(mar=c(2,0,4,0))


    #
    # K
    #
    plot.Avals( expression$K[genes, ], col=3, bg.col=bg.col, main=paste(main, "Kidney"),
                ylabels=F,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl,
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch,
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch,
                unlog.ylabels=unlog.ylabels )

    #
    # L
    #
    plot.Avals( expression$L[genes, ], col=2, bg.col=bg.col, main=paste(main, "Liver"),
                ylabels=F,
                vablines=vablines, vabline.col="lightgrey",
                hablines=hablines, habline.col="lightgrey",
                tcl=tcl,
                ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=lty.cor, type=type, pch=pch,
#                 ylim=ylim, xlab="", ylab="", lty=recycle(lty, length(genes)), lty.cor=is.null(lty), type=type, pch=pch,
                unlog.ylabels=unlog.ylabels )

    #
    # centroid
    #
    if( centroid ) {
        tmp <- get.centroid(lapply(expression[1:3], function(x) x[genes,]))
        #
        # tmp is a matrix with 3 rows, and 31 columns.
        #
        plot.Avals( tmp, col=4:2, bg.col=bg.col, main=paste(main, "average"),
                    ylabels=F,
                    vablines=vablines, vabline.col="lightgrey",
                    hablines=hablines, habline.col="lightgrey",
                    tcl=tcl,
                    ylim=ylim, xlab="", ylab="", unlog.ylabels=unlog.ylabels, lty.cor=lty.cor )
    }
}

## Draw ticks inside the axes pointing inwards, instead of outwards
##
## NB if you override tcl, negative values draws the ticks inside the plot
## region, and +ve values draw ticks outside the plot region.
##
## DEPRECIATED: just use tcl=positive when drawing the axes to make them point
##     inside the plotting region!!
##
## Mark Cowley, 16 March 2006
##
ticks.inplot <- function(sides, tcl=-0.2, labels=F, tick=T, pos=NULL, do.box=F, ...) {
    for(side in sides) {
        # ticks inside the bottom border
        if(side == 1)
            axis(side=3, pos=par("usr")[3], tcl=tcl, labels=labels, tick=tick, ...)
        else if(side == 2)
            # ticks inside left border
            axis(side=4, pos=par("usr")[1], tcl=tcl, labels=labels, tick=tick, ...)
        else if(side == 3)
            # ticks inside top border
            axis(side=1, pos=par("usr")[4], tcl=tcl, labels=labels, tick=tick, ...)
        else if(side == 4)
            # ticks inside right border
            axis(side=2, pos=par("usr")[2], tcl=tcl, labels=labels, tick=tick, ...)
    }

    if(do.box)
        box()

}

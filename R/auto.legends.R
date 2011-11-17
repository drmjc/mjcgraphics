
## functions to automatically place a legend in a plot

## legend.TR <- function(TB.edge=0.02, LR.edge=0.15, ...) {
##     x <- par("usr")[2] - LR.edge*(diff(par("usr")[1:2]))
##     y <- par("usr")[4] - TB.edge*(diff(par("usr")[3:4]))
##     legend(x, y, ...)
## }
##
## legend.TL <- function(TB.edge=0.02, LR.edge=0.02, ...) {
##     x <- par("usr")[1] + LR.edge*(diff(par("usr")[1:2]))
##     y <- par("usr")[4] - TB.edge*(diff(par("usr")[3:4]))
##     legend(x, y, ...)
## }
##
## legend.BR <- function(TB.edge=0.15, LR.edge=0.15, ...) {
##     x <- par("usr")[2] - LR.edge*(diff(par("usr")[1:2]))
##     y <- par("usr")[3] + TB.edge*(diff(par("usr")[3:4]))
##     legend(x, y, ...)
## }
## legend.BR(legend=c("all", "pass threshold", "controls"), col=c(1,2,4), fill=c(1,2,4))
##
## legend.BL <- function(TB.edge=0.15, LR.edge=0.02, ...) {
##     x <- par("usr")[1] + LR.edge*(diff(par("usr")[1:2]))
##     y <- par("usr")[3] + TB.edge*(diff(par("usr")[3:4]))
##     legend(x, y, ...)
## }
## legend.BL(legend=c("all", "pass threshold", "controls"), col=c(1,2,4), fill=c(1,2,4))


legend.TR <- function(legend="", ...) {
    legend("topright", legend=legend, inset=0.02, ...)
}

legend.TL <- function(legend="", ...) {
    legend("topleft", legend=legend, inset=0.02, ...)
}

legend.BR <- function(legend="", ...) {
    legend("bottomright", legend=legend, inset=0.02, ...)
}
## legend.BR(legend=c("all", "pass threshold", "controls"), col=c(1,2,4), fill=c(1,2,4))

legend.BL <- function(legend="", ...) {
    legend("bottomleft", legend=legend, inset=0.02, ...)
}
## legend.BL(legend=c("all", "pass threshold", "controls"), col=c(1,2,4), fill=c(1,2,4))

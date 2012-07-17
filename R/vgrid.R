# Wrapper to 'grid' which just draws the horizontal lines of the grid
#
# see ?grid
#
# Mark Cowley, 4 April 2006
#


#' Wrapper to 'grid' which just draws the horizontal lines of the grid
#' 
#' see ?grid
#' 
#' @author Mark Cowley, 4 April 2006
#' @export
vgrid <- function (nx = NULL, col = "lightgray", lty = "dotted", lwd = NULL, equilogs = TRUE) {
    grid(nx=nx, ny=NA, col=col, lty=lty, lwd=lwd, equilogs=equilogs)
}

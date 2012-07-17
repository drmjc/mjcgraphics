## Function to work out how many rows and plots to make for a certain number of plots.
## eg 25 plots will cause par(mfrow=c(5,5)) to be called.
## The function fills in any unsued plotting spots, eg:
## auto.mfrow(7, TRUE) will set up a device with 3x3 spaces, then when auto.mfrow(7, FALSE) is called
## after the plots have been made, 2 blank plots will then be 'printed'.
##
## NB, you must call auto.mfrow TWICE, once before plotting, and once after plotting, UNLESS
## you know for sure that the nplots specified will fill all of the spaces.
##
## Parameters:
##    nplots: an integer in [1,49]
##    setup: if TRUE, then the graphical parameters (par) is set-up
##           if FALSE, and nplots < the number of spaces for plots in the device, then
##           blank plots are added to fill in the unused spaces.
##
## Mark Cowley, 3 June 2006
##


#' Function to work out how many rows and plots to make for a certain number of
#' plots.
#' 
#' eg 25 plots will cause par(mfrow=c(5,5)) to be called.
#' The function fills in any unsued plotting spots, eg:
#' auto.mfrow(7, TRUE) will set up a device with 3x3 spaces, then when
#' auto.mfrow(7, FALSE) is called
#' after the plots have been made, 2 blank plots will then be 'printed'.
#' NB, you must call auto.mfrow TWICE, once before plotting, and once after
#' plotting, UNLESS
#' you know for sure that the nplots specified will fill all of the spaces.
#' 
#' @param nplots an integer in [1,49]
#' @param setup if TRUE, then the graphical parameters (par) is set-up if
#'   FALSE, and nplots < the number of spaces for plots in the device, then
#'   blank plots are added to fill in the unused spaces.
#' @author Mark Cowley, 3 June 2006
#' @export
auto.mfrow <- function(nplots, setup=TRUE) {

	if(setup) {
		if(nplots <= 3) par(mfrow=c(1, nplots))
		else if(nplots <= 4)  par(mfrow=c(2,2))
		else if(nplots <= 6)  par(mfrow=c(2,3))
		else if(nplots <= 9)  par(mfrow=c(3,3))
		else if(nplots <= 12) par(mfrow=c(3,4))
		else if(nplots <= 16) par(mfrow=c(4,4))
		else if(nplots <= 20) par(mfrow=c(4,5))
		else if(nplots <= 25) par(mfrow=c(5,5))
		else if(nplots <= 30) par(mfrow=c(5,6))
		else if(nplots <= 36) par(mfrow=c(6,6))
		else if(nplots <= 42) par(mfrow=c(6,7))
		else if(nplots <= 49) par(mfrow=c(7,7))
		else if(nplots <= 56) par(mfrow=c(7,8))
		else if(nplots <= 64) par(mfrow=c(8,8))
		else {
			stop("Too many plots")
		}
	}
	else {
		nblankplots <- par("mfrow")[1] * par("mfrow")[2] - nplots
		if(nblankplots > 0)
			for(i in 1:nblankplots)
				plot.blank()
	}
}

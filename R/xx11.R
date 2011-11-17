## Function to open a full size X11 window
##
## Mark Cowley 8 Sept 2005
##
xx11 <- function() {
	grDevices::x11(width=15, height=11)
}

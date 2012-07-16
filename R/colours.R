#' Extend \code{\link[grDevices]{colours}} to return the hex values of the named colours. see
#' \code{\link{colours}}
#' @return a vector of hex values such as \dQuote{#FF0EF0}
#' @author Mark Cowley, 29/3/07
#' @export
colours.hex <- function() {
	colour2hex( colours() )
}

#' Extend \code{\link[grDevices]{colours}} to return the rgb values of the named colours. see
#' \code{\link{colours}}
#' @return a vector of hex values such as ?
#' @author Mark Cowley, 29/3/07
#' @export
colours.rgb <- function() {
	colour2rgb( colours() )
}



########
########
## Check the three different colour formats.
########
########

#' is "col" format
#'
#' Allowable colour Formats:\cr
#' \itemize{
#' \item{col}{examples: White, Black, green}
#' \item{hex}{examples: "#FF00FF", "#FF00FFAE"}
#' \item{rgb}{examples: c(0,255,0)}
#' }
#' @param col a vector of values
#' @return logical: \code{TRUE} if col is in "col" format, \code{FALSE} otherwise.
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' is.colour("white")
#' is.colour("#FFFFFF")
#' is.colour(c(255,255,255))
is.colour <- function(col) {
	is.character(col) && !any(grepl("^#", col))
}

#' is "hex" colour format
#'
#' Allowable colour Formats:\cr
#' \itemize{
#' \item{col}{examples: White, Black, green}
#' \item{hex}{examples: "#FF00FF", "#FF00FFAE"}
#' \item{rgb}{examples: c(0,255,0)}
#' }
#' @param col a vector of values
#' @return logical: \code{TRUE} if col is in "col" format, \code{FALSE} otherwise.
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' is.hex.colour("white")
#' is.hex.colour("#FFFFFF")
#' is.hex.colour(c(255,255,255))
is.hex.colour <- function(hex) {
	is.character(hex) && any(grepl("^#", hex))
}

#' is "rgb" colour format
#'
#' Allowable colour Formats:\cr
#' \itemize{
#' \item{col}{examples: White, Black, green}
#' \item{hex}{examples: "#FF00FF", "#FF00FFAE"}
#' \item{rgb}{examples: c(0,255,0)}
#' }
#' @param col a vector of values
#' @return logical: \code{TRUE} if col is in "col" format, \code{FALSE} otherwise.
#' @author Mark Cowley, 2011-10-20
#' @export
#' @examples
#' is.rgb.colour("white")
#' is.rgb.colour("#FFFFFF")
#' is.rgb.colour(c(255,255,255))
is.rgb.colour <- function(rgb) {
	( is.matrix.like(rgb) && ncol(rgb) >= 3 ) ||
	( is.numeric(rgb) && length(rgb) >= 3 )
}

################################################################################
################################################################################
## Convert from one format to another.
################################################################################
################################################################################
# colour2rgb exists in grDevices
# colour2rgb
# rgb2rgb
# rgb2colour
# rgb2hex
# rgb2colour
#


#' Convert "col" format to "rgb" format.
#' 
#' ie from "White" to c(255,255,255)
#' 
#' @param x a vector of colours in the colour format: \dQuote{white}
#' @author Mark Cowley, 29/3/07
#' @export
#' @examples
#' colour2rgb("White")
#' # [1] c(255,255,255)
colour2rgb <- function(x=NULL) {
	stopifnot( is.colour(x) )

	rgb <- t(grDevices::col2rgb(x))
	rownames( rgb ) <- x
	rgb
}

#' Convert "col" format to "rgb" format.
#' 
#' ie from "White" to "#FFFFFF"
#' 
#' @param x a vector of colours in the colour format: \dQuote{white}
#' @author Mark Cowley, 29/3/07
#' @export
#' @examples
#' colour2hex("White")
#' # [1] "#FFFFFF"
colour2hex <- function(x=NULL) {
	stopifnot( is.colour(x) )

	rgb <- colour2rgb( x )
	hex <- rgb2hex( rgb )
	names( hex ) <- x
	hex
}


#' Convert "hex" format to "rgb" format.
#' 
#' ie from "#FF00EO" to c(256,0,224)
#' 
#' @param x a vector of colours in the hex format: \dQuote{#FF00FF}
#' @author Mark Cowley, 29/3/07
#' @export
#' @examples
#' hex2rgb( "#FF00EO" )
#' # [1] c(256,0,224)
hex2rgb <- function(x=NULL) {
	stopifnot( is.hex.colour(x) )

	# trim the hash
	x <- substring(x,2,7)
	rgb <- cbind(red=as.numeric(paste0("0x",substring(x,1,2))),
				 green=as.numeric(paste0("0x",substring(x,3,4))),
				 blue=as.numeric(paste0("0x",substring(x,5,6))))
	if( !is.null(names(x)) )
		rownames(rgb) <- names(x)
	rgb
}


#' Convert "rgb" format to "hex" format.
#' 
#' ie from c(255,255,255) to "#FFFFFF"
#' 
#' @param x a vector of colours in the "rgb format" format; ie has three
#'   columns of [0,255] values, one colour per row.
#' @author Mark Cowley, 29/3/07
#' @export
#' @examples
#' rgb2hex( matrix(c(255,255,255),1,3) )
#' # [1] #FFFFFF
rgb2hex <- function(x) {
	stopifnot( is.rgb.colour(x) )

	# convert an int in [0-255] into a character hex string 0 -> 00; 7 -> 07; 255 -> FF
	.tohex2 <- function(x) {
		x <- as.character.hexmode(x, upper.case=TRUE)
		idx <- nchar(x)<2
		if( any(idx) )
			x[idx] <- paste("0", x[idx], sep="")
		x
	}
	cols <- apply(x, 1, function(x) paste(c("#", .tohex2(x)), collapse=""))

	cols
}


#' Convert "hex" format to "col" format.
#' 
#' ie from "#FFFFFF" to "White"
#' Note, there are 256^3 possible x's but only length(colours())
#' possible outputs, so the closest colour for each x is determined.
#' This function is the opposite of grDevices:col2rgb, and also is
#' the opposite of colour2rgb
#' 
#' @param x a vector of colours in the hex format: \dQuote{#FF00FF}
#' @param cols.rgb the pool of colours with which to map \code{x} to in rgb format.
#'   Leave as default to get the normal "R colours". NB: If \code{cols.rgb}
#'   doesn't have names, then the index into cols will be
#'   returned. If \code{cols.rgb} has valid names, then they will be returned.
#' @return a vector of the name of R colours; eg \code{c("white", "magenta",
#'   "lightblue")} or a vector of indices into the \code{cols.rgb} vector, if
#'    no valid names were found in the \code{cols.rgb} object.
#' @author Mark Cowley, 29/3/07
#' @export
#' @examples
#' hex2colour( "#FFFFFF" )
#' # [1] "white"
hex2colour <- function(x, cols.rgb=colours.rgb()) {
	stopifnot( is.hex.colour(x) )
	# stopifnot( !is.null(rownames(cols.rgb)) )

	rgb <- hex2rgb(x)

	col <- rgb2colour(rgb, cols.rgb)
	col
}

#' Convert "rgb" format to "col" format.
#' 
#' ie from "c(255,255,255)" to "White"
#' Note,there are 256^3 possible x's but only length(cols.rgb())
#' possible outputs, so the closest colour for each x is determined.
#' This function is the opposite of grDevices:col2rgb, and also is
#' the opposite of colour2rgb
#' 
#' @param x a vector of colours in the "rgb format" format; ie has three
#'   columns of [0,255] values, one colour per row.
#' @param cols.rgb the pool of colours with which to map x to in rgb format.
#'   Leave as default to get the normal "R colours". NB: If \code{cols.rgb}
#'  doesn't have names, then the index into cols will be
#'   returned. If \code{cols.rgb} has valid names, then they will be returned.
#' @return a vector of the name of R colours;\cr
#' eg c("white", "magenta", "lightblue"), or a vector of indices into the 
#' \code{cols.rgb} vector, if no valid names were found in the \code{cols.rgb} object.
#' @author Mark Cowley, 29/3/07
#' @export
rgb2colour <- function(x, cols.rgb=colours.rgb()) {
	stopifnot( is.rgb.colour(x) )
	# stopifnot( !is.null(rownames(cols.rgb)) )

	res <- rep(0, nrow(x))
	for(i in 1:nrow(x)) {
		tmp <- rbind(x[i,], cols.rgb)
		tmpdist <- as.matrix( dist(tmp, method = "euclidean") )[1, 2:nrow(cols.rgb)]
		res[i] <- which.min(tmpdist)
	}
	if( !is.null( rownames(cols.rgb) ) ) {
		res <- rownames(cols.rgb)[res]
	}

	res
}


#' Return the inverse of a colour in hex form so that it can be passed directly
#' to a plot command.
#' 
#' @param col a vector of colours to be reversed
#' @return a vector of colours
#' @examples
#' plot(1:10, col=inverse.colour("purple"))
#' inverse.colour("red")
#' # #00FFFF
#' inverse.colour("green")
#' # #FF00FF
#' inverse.colour("blue")
#' # #FFFF00
#' 
#' @author Mark Cowley, 16 May 2006
#' @export
inverse.colour <- function(col) {
   rgb <- colour2rgb(col)
   rgb <- abs(rgb - rep(255,3))
   rgb2hex(rgb)
   # rgb(rgb[1], rgb[2], rgb[3], maxColorValue=255)
}


#' Make a gradient of colours.
#' Evenly make \code{steps} colours merging from the colour \code{from} to the colour
#' \code{to}, optionally via the colour \code{via}
#' 
#' @param from the first, or starting colour
#' @param to the final colour
#' @param via [optional] the middle colour. Leave as \code{NULL}, to ignore this.
#' @param steps numeric: how many colours do you want?
#' @param alpha numeric in [0,255] controlling the opacity, from trasparent (0)
#'  to opaque (255), or in [0.0,1.0].
#' @author Mark Cowley, 24 August, 2005
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#' @export
#' @examples
#' colour.step(from="green", to="red", via="black", steps=3)
colour.step <- function(from="black", to="red", via=NULL, steps=10, alpha=255) {
   if( !is.null(via) ) {
		#
		# split the intervals into two, then merge the results
		#
		if( odd(steps) )
			n <- steps/2+0.5
		else
			n <- steps/2 + 1
		res <- c(colour.step(from=from, to=via, via=NULL, steps=n)[1:n],
				 colour.step(from=via, to=to, via=NULL, steps=n)[2:n])

		if( even(steps) )
			# the nth element is the "via" colour which can never be
			# reached, when using an even number of steps.
			res <- res[-n]

		return( res )
	}
	else {
		if( is.colour(from) ) {
			Frgb <- colour2rgb(from)/255
			Trgb <- colour2rgb(to)/255
		}
		else if( is.hex.colour(from) ) {
			Frgb <- hex2rgb(from)/255
			Trgb <- hex2rgb(to)/255
		}
		else {
			Frgb <- from/255
			Trgb <- to/255
		}
		res <- rgb( seq(Frgb[1], Trgb[1], length.out=steps),
					seq(Frgb[2], Trgb[2], length.out=steps),
					seq(Frgb[3], Trgb[3], length.out=steps))

		if( alpha >= 0.0 && alpha <= 1.0 )
			alpha <- round(alpha*255,0)

		if( alpha < 0 )
			alpha <- 0
		else if( alpha > 255 )
			alpha <- 255
		if( is.numeric(alpha) )
			alpha <- toupper(format.hexmode(alpha))
		res <- paste(res, alpha, sep="")
		return(res)
	}
}
# CHANGELOG
# 24 August 2005 - v1
# 18/7/07
# - fixed  this up massively to produce 'steps' colours<
# and now using via works properly.
# NB, if using a "via" colour, then to actually see that via colour,
# you need an odd number of steps.
# 4/12/07
# -added alpha, to allow different alpha
#	alpha can be in [0.0, 1.0] or in [0,255] If it's outside these values,
#	it is set to #00, or #FF



#' colours.mjc
#' Produce a vector of colours, following a number of colouring schemes.
#'
#' @param palette one of a number of named palette's. see usage.
#' @param N the number of colours
#' @return a vector of N colours, in hex format
#' @author Mark Cowley, 2011-10-20
#' @export
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#' @examples
#' colours.mjc("excel07")
#' colours.mjc("reds", 7)
colours.mjc <- function(palette=c("excel07", "gp", "rgb", "reds", "greens", "blues", "oranges", "cyans", "purples",
	"red2green"), N=5) {
	palette <- palette[1]
	if( palette == "excel07" ) {
		recycle(c('#40699C', '#9E413E', '#7F9A48', '#695185', '#3C8DA3', '#CC7B38', '#AABAD7', '#B3B3B3'), N)
	}
	else if( palette == "gp" ) {
		colour.step("#4500AD", "#D60C00", via="#FFFFFF", steps=N+even(N))
	}
	else if( palette == "rgb" ) {
		recycle(c("#7F3230","#657B38", "#31547D"), N)
	}
	else if( palette == "reds" ) {
		colour.step(from="#7F3230", to="#E8D0D0", steps=N)
	}
	else if( palette == "greens" ) {
		colour.step(from="#657B38", to="#DEE7D1", steps=N)
	}
	else if( palette == "blues" ) {
		colour.step(from="#31547D", to="#D0D8E8", steps=N)
	}
	else if( palette == "oranges" ) {
		colour.step(from="#A4622B", to="#FCDDCF", steps=N)
	}
	else if( palette == "cyans" ) {
		colour.step(from="#2F7183", to="#D0E3EA", steps=N)
	}
	else if( palette == "purples" ) {
		colour.step(from="#53406A", to="#D8D3E0", steps=N)
	}
	else if( palette == "red2green" ) {
		colour.step("#7F3230", "#657B38", "#FFFFFF", N+even(N))
	}
}


test.colours.mjc <- function(N=8, data=rnorm(N,4)) {
	par(mfrow=c(2,4))
	barplot(data, col=colours.mjc("excel07", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("rgb", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("reds", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("greens", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("blues", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("oranges", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("cyans", N=N), main="", border=NA)
	barplot(data, col=colours.mjc("purples", N=N), main="", border=NA)
}

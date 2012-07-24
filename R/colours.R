#' @name mjc_colours
#' @title colours, hex-colours and rgb-colours
#' @description A collection of functions to convert colours from names (aka \code{colours}),
#' hex values (aka \code{hex}) and RGB 3 character numeric's (aka \code{rgb}).
#' This is \emph{old} code, and is crying out for at least some S3-class love. It was written
#' to facilitate the interpolation of lots of colours, which requires named colours to be 
#' numerically represented. This code allows \code{\link{colour.step}} to do its thing, though
#' newer code in the \code{RColorBrewer} package may make much of this redundant.
#' 
#' @section Allowed colour Formats:
#' We've named the default colours provided by grDevices the \code{col} format, for want of a
#' better name.
#' \itemize{
#' \item{col:}{"White", "white", "Black", "green"}
#' \item{hex:}{"#FF00FF", "#FF00FFAE"}
#' \item{rgb:}{matrix(c(0,255,0), 1, 3)}
#' }
#' 
#' @details 
#' grDevices provides \code{\link[grDevices]{colours}}, and \code{\link[grDevices]{col2rgb}}.
#' We wanted a hexadecimal representation of
#' colours which included an alpha channel, as well as methods for checking colours, converting
#' between colour types, and generating a spectrum of colours.
#' 
#' @section colour types:
#' \code{colours}, \code{colours.hex}, \code{colours.rgb}: 
#' return the hex, or rgb representations of the named colours provided by 
#' \code{\link[grDevices]{colours}} from \code{grDevices}.
#' 
#' @section colour checks:
#' \code{is.colour}, \code{is.hex.colour}, \code{is.rgb.colour}: 
#' methods
#' for checking whether the input is one of the 3 colour types, the hex colour type, or rgb
#' colour type, respectively.
#'
#' @section colour conversion:
#' \code{colour2rgb}, \code{colour2hex}: convert from \code{col} format\cr
#' \code{hex2rgb}, \code{hex2colour}: convert from hex format\cr
#' \code{rgb2hex}, \code{rgb2colour}: convert from rgb format\cr
#' Note, \code{hex2colour}: there are 256^3 possible hex's but only \code{length(colours())}
#' possible outputs, so the closest colour for each \code{hex} is determined.
#' This function is the opposite of \code{grDevices:col2rgb}, and also is
#' the opposite of \code{colour2rgb}\cr
#' Note, \code{rgb2colour}: there are 256^3 possible x's but only length(cols.rgb())
#' possible outputs, so the closest colour for each x is determined.
#' This function is the opposite of grDevices:col2rgb, and also is
#' the opposite of colour2rgb
#' 
#' @section \code{inverse.colour}:
#' inverse a \code{col}-format colour & return the \code{hex}-format colour
#' 
#' @param col a vector of values in the \code{colour} format: eg \dQuote{white}
#' @param rgb a vector of colours in the \code{rgb} format; ie a 3-column matrix
#'  of [0,255] values, one colour per row.
#' @param hex a vector of colours in the hex format: eg \dQuote{#FF00FF}
#' @param cols.rgb the pool of colours with which to map \code{hex} to in rgb format.
#'   Leave as default to get the normal "R colours". NB: If \code{cols.rgb}
#'   doesn't have names, then the index into cols will be
#'   returned. If \code{cols.rgb} has valid names, then they will be returned.
#' 
#' @return \code{colours}, \code{colours.hex}, \code{colours.rgb}: a vector of
#' col-format colours, hex-format colours, or matrix of rgb-format colours, 1 row per colour.\cr
#' \cr
#' \code{is.colour}, \code{is.hex.colour}, \code{is.rgb.colour}: logical.\cr
#' \cr
#' \code{colour2rgb}, \code{colour2hex}, \code{hex2rgb}, \code{rgb2hex}, \code{hex2colour}, \code{rgb2colour}: a vector of colour formats, converted from one to another.\cr
#' \cr
#' \code{hex2colour}: a vector of the name of R colours; eg \code{c("white", "magenta",
#'   "lightblue")} or a vector of indices into the \code{cols.rgb} vector, if
#'    no valid names were found in the \code{cols.rgb} object.
#' \code{rgb2colour}: a vector of the name of R colours;
#' eg \code{c("white", "magenta", "lightblue")}, or a vector of indices into the 
#' \code{cols.rgb} vector, if no valid names were found in the \code{cols.rgb} object.\cr
#' \cr
#' \code{inverse.colour}: a vector of colours\cr
#' 
#' 
#' @rdname mjc-colours
#' @author Mark Cowley, 29/3/07
#' 
#' @examples
#' colours()[1:5]
#' # [1] "white"         "aliceblue"     "antiquewhite"  "antiquewhite1"
#' # [5] "antiquewhite2"
#' colours.hex()[1:5]
#' #     white     aliceblue  antiquewhite antiquewhite1 antiquewhite2 
#' # "#FFFFFF"     "#F0F8FF"     "#FAEBD7"     "#FFEFDB"     "#EEDFCC" 
#' colours.rgb()[1:5,]
#' #               red green blue
#' # white         255   255  255
#' # aliceblue     240   248  255
#' # antiquewhite  250   235  215
#' # antiquewhite1 255   239  219
#' # antiquewhite2 238   223  204
#' 
#' is.colour("white") # TRUE
#' is.colour("#FFFFFF") # FALSE
#' is.colour(c(255,255,255)) # FALSE
#' 
#' is.hex.colour("white") # FALSE
#' is.hex.colour("#FFFFFF") # TRUE
#' is.hex.colour(c(255,255,255)) # FALSE
#' 
#' is.rgb.colour("white") # FALSE
#' is.rgb.colour("#FFFFFF") # FALSE
#' is.rgb.colour(c(255,255,255)) # TRUE
#' 
#' colour2rgb("White")
#' #       red green blue
#' # White 255   255  255
#' colour2hex("White")
#' #     White 
#' # "#FFFFFF" 
#' hex2rgb( "#FF00E0" )
#' #      red green blue
#' # [1,] 255     0  224
#' rgb2hex( matrix(c(255,128,255),1,3) )
#' # [1] #FF80FF
#' hex2colour( "#FF80FF" )
#' # [1] "orchid1"
#' rgb2colour(matrix(c(255,128,255), 1, 3))
#' # [1] "orchid1"
#' 
#' inverse.colour("red")
#' #       red 
#' # "#00FFFF" 
#' inverse.colour("orchid1")
#' #   orchid1 
#' # "#007C05"
NULL

#' @export
#' @rdname mjc-colours
colours.hex <- function() {
	colour2hex( colours() )
}

#' @export
#' @rdname mjc-colours
colours.rgb <- function() {
	colour2rgb( colours() )
}

#' @export
#' @rdname mjc-colours
is.colour <- function(col) {
	is.character(col) && !any(grepl("^#", col))
}

#' @export
#' @rdname mjc-colours
is.hex.colour <- function(hex) {
	is.character(hex) && any(grepl("^#", hex))
}

#' @export
#' @rdname mjc-colours
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

#' @export
#' @rdname mjc-colours
colour2rgb <- function(col=NULL) {
	stopifnot( is.colour(col) )

	rgb <- t(grDevices::col2rgb(col))
	rownames( rgb ) <- col
	rgb
}

#' @export
#' @rdname mjc-colours
colour2hex <- function(col=NULL) {
	stopifnot( is.colour(col) )

	rgb <- colour2rgb( col )
	hex <- rgb2hex( rgb )
	names( hex ) <- col
	hex
}

#' @export
#' @rdname mjc-colours
hex2rgb <- function(hex=NULL) {
	stopifnot( is.hex.colour(hex) )

	# trim the hash
	hex <- substring(hex,2,7)
	rgb <- cbind(red=as.numeric(paste0("0x",substring(hex,1,2))),
				 green=as.numeric(paste0("0x",substring(hex,3,4))),
				 blue=as.numeric(paste0("0x",substring(hex,5,6))))
	if( !is.null(names(hex)) )
		rownames(rgb) <- names(hex)
	rgb
}

#' @export
#' @rdname mjc-colours
rgb2hex <- function(rgb) {
	stopifnot( is.rgb.colour(rgb) )

	# convert an int in [0-255] into a character hex string 0 -> 00; 7 -> 07; 255 -> FF
	.tohex2 <- function(rgb) {
		rgb <- as.character.hexmode(rgb, upper.case=TRUE)
		idx <- nchar(rgb)<2
		if( any(idx) )
			rgb[idx] <- paste("0", rgb[idx], sep="")
		rgb
	}
	cols <- apply(rgb, 1, function(rgb) paste(c("#", .tohex2(rgb)), collapse=""))

	cols
}


#' @export
#' @rdname mjc-colours
hex2colour <- function(hex, cols.rgb=colours.rgb()) {
	stopifnot( is.hex.colour(hex) )
	# stopifnot( !is.null(rownames(cols.rgb)) )

	rgb <- hex2rgb(hex)

	col <- rgb2colour(rgb, cols.rgb)
	col
}

#' @export
#' @rdname mjc-colours
rgb2colour <- function(rgb, cols.rgb=colours.rgb()) {
	stopifnot( is.rgb.colour(rgb) )
	# stopifnot( !is.null(rownames(cols.rgb)) )

	res <- rep(0, nrow(rgb))
	for(i in 1:nrow(rgb)) {
		tmp <- rbind(rgb[i,], cols.rgb)
		tmpdist <- as.matrix( dist(tmp, method = "euclidean") )[1, 2:nrow(cols.rgb)]
		res[i] <- which.min(tmpdist)
	}
	if( !is.null( rownames(cols.rgb) ) ) {
		res <- rownames(cols.rgb)[res]
	}

	res
}


#' @export
#' @rdname mjc-colours
inverse.colour <- function(col) {
   rgb <- colour2rgb(col)
   rgb <- abs(rgb - rep(255,3))
   rgb2hex(rgb)
   # rgb(rgb[1], rgb[2], rgb[3], maxColorValue=255)
}


#' Make a gradient of colours.
#' 
#' Evenly make \code{steps} colours merging from the colour \code{from} to the colour
#' \code{to}, optionally via the colour \code{via}
#' 
#' @param from the first, or starting colour
#' @param to the final colour
#' @param via [optional] the middle colour. Leave as \code{NULL}, to ignore this.
#' @param steps numeric: how many colours do you want?
#' @param alpha numeric in [0,255] controlling the opacity, from trasparent (0)
#'  to opaque (255), or in [0.0,1.0].
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#' @export
#' @author Mark Cowley, 2005-08-24
#' @examples
#' colour.step(from="green", to="red", via="black", steps=11)
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
#' 
#' Produce a vector of colours, following a number of colouring schemes.
#'
#' @param palette one of a number of named palette's. see usage.
#' @param N the number of colours
#' @return a vector of N colours, in hex format
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

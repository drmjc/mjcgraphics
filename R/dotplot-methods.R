#' dotplot methods
#' 
#' The S3 method \code{\link[graphics]{plot}} is undoubtedly a workhorse. 
#' It sometimes makes dotplots, barplots, boxplots\dots
#' This S4 method makes it explicit that you will get a \code{dotplot}, either 1D, or 2D.
#' It can then be overridden when appropriate to define dotplots for new data types,
#' like dotplot on ExpressionSet's from my microarrays library.
#'
#' @param x an object to dotplot
#' @param y an optional object to dotplot
#' @param sort logical: sort 'x'. ignored for 2D plots.
#' 
#' @return nothing. makes a plot
#' 
#' @author Mark Cowley
#' 
#' @exportMethod dotplot
#' @rdname dotplot-methods
#' @docType methods
#' 
#' @examples
#' # 1D
#' dotplot(1:10)
#' dotplot(rnorm(100))
#' ## dotplot(letters)
#' # 2D
#' dotplot(1:100, sqrt(1:100))
#' a <- 1:10; names(a) <- letters[1:10]
#' b <- 6:15; names(b) <- letters[6:15]
#' dotplot(a,b)
#' # should issue a message
#' 
#' c <- 6:16; names(c) <- letters[6:16]
#' dotplot(a,c)
#' 
#' d <- 11:20; names(d) <- LETTERS[1:10]
#' dotplot(a,d)
#' # issues a message
#' 
#' ## dotplot(letters, 1:10)
#' ## dotplot(1:10, letters)
setGeneric(
	"dotplot",
	function(x, y, sort=FALSE, ...) {
		standardGeneric("dotplot")
	}
)

#' @rdname dotplot-methods
#' @aliases dotplot,numeric,missing,missing-method
setMethod(
	"dotplot",
	signature=signature("numeric", "missing", "missing"),
	function(x, y, sort, ...) {
		dotplot(x, sort=FALSE, ...)
	}
)

#' @rdname dotplot-methods
#' @aliases dotplot,numeric,missing,logical-method
setMethod(
	"dotplot",
	signature=signature("numeric", "missing", "logical"),
	function(x, y, sort, ...) {
		if( sort ) x <- sort(x, decreasing=FALSE)
		plot(x, ...)
	}
)

#' @rdname dotplot-methods
#' @aliases dotplot,character,ANY,ANY-method
setMethod(
	"dotplot",
	signature=signature("character", "ANY", "ANY"),
	function(x, y, sort=FALSE, ...) {
		stop("unsupported data type")
	}
)

#' @rdname dotplot-methods
#' @aliases dotplot,ANY,character,ANY-method
setMethod(
	"dotplot",
	signature=signature("ANY", "character", "ANY"),
	function(x, y, sort=FALSE, ...) {
		stop("unsupported data type")
	}
)

################################################################################
###############  2D methods ####################################################
################################################################################

#' @rdname dotplot-methods
#' @aliases dotplot,numeric,numeric,logical-method
setMethod(
	"dotplot",
	signature=signature("numeric", "numeric", "logical"),
	function(x, y, sort, ...) {
		#
		# find overlaps
		# 
		if( length(x) == length(y) ) {
			if( !is.null(names(x)) && !is.null(names(y)) && !all(names(x)==names(y)) ) {
				if (sum(names(x) %in% names(y)) > 1) {
					message("careful, x and y have same lengths, but their names differ; there are some names that overlap, so reordering & sub-setting x & y to match the names.")
					common <- intersect(names(x), names(x))
					x <- x[common]
					y <- y[common]
				}
				else {
					message("careful, x and y have same lengths, but none of the names overlap. ignoring the names.")
				}
			}
		}
		else {
			if( !is.null(names(x)) && !is.null(names(y)) ) {
				common <- intersect(names(x), names(x))
				length(common) > 1 || stop("x and y lengths differ, and no names found in common")
				x <- x[common]
				y <- y[common]
			}
			else {
				stop("x and y lengths differ, and they don't have names")
			}
		}
		plot(x, y, ...)
	}
)
#' @rdname dotplot-methods
#' @aliases dotplot,numeric,numeric,missing-method
setMethod(
	"dotplot",
	signature=signature("numeric", "numeric", "missing"),
	function(x, y, sort, ...) {
		dotplot(x, y, sort=FALSE, ...)
	}
)


################################################################################
################################################################################
################################################################################
# old S3 version:
# #' dotplot methods
# #' 
# #' plot is undoubtedly a workhorse. It sometimes makes dotplots, barplots, boxplots\dots
# #' This method makes it explicit that you will get a \code{dotplot}, either 1D, or 2D.
# #' It can then be overridden when appropriate to define dotplots for new data types,
# #' like dotplot.ExpressionSet from my microarrays library.
# #'
# #' @param x an object to plot
# #' @param \dots arguments passed to specific methods.
# #' @return none. a plot is made.
# #' @author Mark Cowley, 2012-11-14
# #' 
# #' @S3method dotplot default
# #' @S3method dotplot numeric
# #' @S3method dotplot character
# #' @rdname dotplot-methods
# dotplot <- function(x, ...) UseMethod("dotplot")
# dotplot.default <- function(x, ...) { plot(x, ...) }
# dotplot.numeric <- function(x, ...) { plot(x, ...) }
# dotplot.character <- function(x, ...) { stop("unsupported input type") }
# 

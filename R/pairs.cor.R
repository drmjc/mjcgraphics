#' Print the correlation in a panel
#' 
#' Print the correlation in a panel, with size proportional to the magnitude
#' of correlation. Taken from the examples within \code{\link[graphics]{pairs}}
#' 
#' @param x a numeric vector
#' @param y a numeric vector
#' @param digits the number of digits to display
#' @param prefix the prefix to add to the correlation score. Default=\dQuote{}
#' @param cex.cor the character expansion value. Default=missing == 0.8/strwidth(txt)
#' @return alters a plotting panel
#' @author Mark Cowley, 2008-12-15
#' @seealso \code{\link{pairs.cor}}, \code{\link[graphics]{pairs}}
#' @export
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r <- abs(cor(x, y, use="complete.obs"))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex.cor * r)
}

#' pairs plot with correlation stats
#' 
#' A wrapper to pairs with a correlation value in lower triangle, and a
#' smoothed dot plot in upper triangle
#' 
#' @param x the coordinates of points given as numeric columns of a
#'		  matrix or dataframe.	Logical and factor columns are
#'		  converted to numeric in the same way that data.matrix does.
#' @param \dots arguments passed to \code{\link[graphics]{pairs}}
#' @param upper.panel which function to use in the upper plotting panel?
#'	 default = \code{panel.smooth}
#' @param lower.panel which function to use in the lower plotting panel?
#'	 default = \code{\link{panel.cor}}
#' @author Mark Cowley, 2008-12-15
#' @export
pairs.cor <- function(x, ..., upper.panel=panel.smooth, lower.panel=panel.cor) {
	pairs(x, ..., upper.panel=upper.panel, lower.panel=lower.panel)
}


#' panel containing xy plot and linear model fit line
#' 
#' panel, for use with pairs, containing xy plot and linear model fit line.
#'
#' @param x numeric vectors of the same length
#' @param y numeric vectors of the same length
#' @param col see \code{\link[graphics]{par}}
#' @param bg see \code{\link[graphics]{par}}
#' @param pch see \code{\link[graphics]{par}}
#' @param cex see \code{\link[graphics]{par}}
#' @param col.smooth color to be used by \code{lines} for drawing the smooths.
#' @param span smoothing parameter \code{f} for \code{lowess}, see there.
#' @param iter number of robustness iterations for \code{lowess}.
#' @param col.lm the colour of the linear model line
#' @param \dots arguments to control the appearance of the line, passed to
#'  \code{\link{abline}}
#' 
#' @return none.
#' 
#' @seealso \code{\link[graphics]{panel.smooth}} \code{\link[graphics]{pairs}}
#' @author Mark Cowley, 2012-06-27
#' @export
#' 
#' @examples
#' pairs(swiss, panel = panel.lm, pch = ".")
#' pairs(swiss, panel = panel.lm, lwd = 2, cex= 1.5, col="blue")# hmm...
panel.lm <- function(x, y, lty="solid", col=par("col"), bg=NA, pch=par("pch"), 
	cex=1, col.lm="red", ...) {
	
	points(x, y, pch = pch, col = col, bg = bg, cex = cex)
	ok <- is.finite(x) & is.finite(y)
	if (any(ok)) {
		fit <- lm(y[ok] ~ x[ok])
		abline(fit, lty=lty, col=col.lm, ...)
	}
}

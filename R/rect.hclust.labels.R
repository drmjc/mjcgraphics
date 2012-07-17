# Annotate the rectangles produced by rect.hclust using numbers.
#
# Why? plclust is great for plotting heirarchical clusters; rect.hclust is great for selecting clusters at a given distance threshold; what is lacking is knowing which clusters are which. This is espescially useful if you then do some downstream analysis on each cluster, if you end up referring to your clusters numerically.
#
# Parameters:
#	tree, k, which, x, h, border, cluster: see rect.hclust. NB this must be identical to those used in your call to rect.hclust
#
# Value:
#	annotates a plot with indices
#
# Examples:
# m <- matrix(rnorm(200), nrow=20)
# hc <- hclust(dist(m))
# plot(hc)
# rect.hclust(hc, c=5)
# rect.hclust.labels(hc, c=5)
#
# Mark Cowley, 2009-10-30
#


#' Annotate the rectangles produced by rect.hclust using numbers.
#' 
#' Why? plclust is great for plotting heirarchical clusters; rect.hclust is
#' great for selecting clusters at a given distance threshold; what is lacking
#' is knowing which clusters are which. This is espescially useful if you then
#' do some downstream analysis on each cluster, if you end up referring to your
#' clusters numerically.
#' 
#' @param tree see rect.hclust. NB this must be identical to those used in your
#'   call to rect.hclust
#' @param k see rect.hclust. NB this must be identical to those used in your
#'   call to rect.hclust
#' @param which see rect.hclust. NB this must be identical to those used in
#'   your call to rect.hclust
#' @param x see rect.hclust. NB this must be identical to those used in your
#'   call to rect.hclust
#' @param h see rect.hclust. NB this must be identical to those used in your
#'   call to rect.hclust
#' @param border see rect.hclust. NB this must be identical to those used in
#'   your call to rect.hclust
#' @param cluster see rect.hclust. NB this must be identical to those used in
#'   your call to rect.hclust
#' @return annotates a plot with indices
#' @author Mark Cowley, 2009-10-30
#' @examples
#' m <- matrix(rnorm(200), nrow=20)
#' hc <- hclust(dist(m))
#' plot(hc)
#' rect.hclust(hc, k=5)
#' rect.hclust.labels(hc, k=5)
#' @export
rect.hclust.labels <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, cluster = NULL) {
	# begin code from rect.hclust
	if (length(h) > 1 | length(k) > 1) 
		stop("'k' and 'h' must be a scalar")
	if (!is.null(h)) {
		if (!is.null(k)) 
			stop("specify exactly one of 'k' and 'h'")
		k <- min(which(rev(tree$height) < h))
		k <- max(k, 2)
	}
	else if (is.null(k)) 
		stop("specify exactly one of 'k' and 'h'")
	if (k < 2 | k > length(tree$height)) 
		stop(gettextf("k must be between 2 and %d", length(tree$height)), 
			domain = NA)
	if (is.null(cluster)) 
		cluster <- cutree(tree, k = k)
	clustab <- table(cluster)[unique(cluster[tree$order])]
	m <- c(0, cumsum(clustab))
	if (!is.null(x)) {
		if (!is.null(which)) 
			stop("specify exactly one of 'which' and 'x'")
		which <- x
		for (n in 1L:length(x)) which[n] <- max(which(m < x[n]))
	}
	else if (is.null(which)) 
		which <- 1L:k
		
	if (any(which > k)) 
		stop(gettextf("all elements of 'which' must be between 1 and %d", k), domain = NA)
	# end code from rect.hclust
	
	n <- length(which)
	xpos <- rowMeans(cbind(m[which[1:n]]+0.66,    m[which[1:n]+1]+0.33))
	ypos <- rep(par("usr")[3L], n)
	text(xpos, ypos, n:1, adj=c(0,0), pos=3, col="red", cex=0.75)
	warning("Due to the stochastic nature of hclustering, the clusters from cutree may be in a different order to the labels.\n")

}


# # V1: In order to add labels to an hclust, I over-wrote rect.hclust, but opted in favour of rect.hclust.labels.
# #
# #
# rect.hclust <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, cluster = NULL, labels=TRUE) {
# 	if (length(h) > 1 | length(k) > 1) 
# 		stop("'k' and 'h' must be a scalar")
# 	if (!is.null(h)) {
# 		if (!is.null(k)) 
# 			stop("specify exactly one of 'k' and 'h'")
# 		k <- min(which(rev(tree$height) < h))
# 		k <- max(k, 2)
# 	}
# 	else if (is.null(k)) 
# 		stop("specify exactly one of 'k' and 'h'")
# 	if (k < 2 | k > length(tree$height)) 
# 		stop(gettextf("k must be between 2 and %d", length(tree$height)), 
# 			domain = NA)
# 	if (is.null(cluster)) 
# 		cluster <- cutree(tree, k = k)
# 	clustab <- table(cluster)[unique(cluster[tree$order])]
# 	m <- c(0, cumsum(clustab))
# 	if (!is.null(x)) {
# 		if (!is.null(which)) 
# 			stop("specify exactly one of 'which' and 'x'")
# 		which <- x
# 		for (n in 1L:length(x)) which[n] <- max(which(m < x[n]))
# 	}
# 	else if (is.null(which)) 
# 		which <- 1L:k
# 		
# 	if (any(which > k)) 
# 		stop(gettextf("all elements of 'which' must be between 1 and %d", k), domain = NA)
# 		
# 	border <- rep(border, length.out = length(which))
# 	# labels.text <- sprintf("cluster%03d", 1L:length(which))
# 	retval <- list()
# 	for (n in 1L:length(which)) {
# 		rect(m[which[n]] + 0.66,
# 			 par("usr")[3L], 
# 			 m[which[n] + 1] + 0.33, 
# 			 mean(rev(tree$height)[(k - 1):k]), border = border[n])
# 		retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
# 		# if( labels ) {
# 		# 	# text( mean(c(m[which[n]]+0.66, m[which[n+1]]+0.33)), par("usr")[3L],
# 		# 	# 	labels.text[n], adj=c(0,0), srt=90, col="red")
# 		# 	text( 
# 		# 		mean(c(m[which[n]]+0.66, m[which[n]+1]+0.33)), par("usr")[3L], 
# 		# 		n,
# 		# 		adj=c(0,0), pos=3, col="red"
# 		# 	 )
# 		# }
# 	}
# 	if( labels ) {
# 		n <- length(which)
# 		xpos <- rowMeans(cbind(m[which[1:n]]+0.66,    m[which[1:n]+1]+0.33))
# 		ypos <- rep(par("usr")[3L], n)
# 		text(xpos, ypos, 1:n, adj=c(0,0), pos=3, col="red")
# 	}
# 
# 	invisible(retval)
# }

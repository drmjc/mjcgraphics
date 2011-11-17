## Function to plot a histogram of the counts for each word in a vector of words.
## The histogram is ordered such that the first word seen is in first bar of histogram etc...
##
## Parameters:
##     words: the vector of words
##     x.labels: if T, plot the word under each of the bars; if F, plot numbers
##     na.rm: remove NA's if there are any in the words list.
##     xlab, main, xaxt: see par
## Value:
##     nil: used for its side effect of plotting a histogram of word counts.
##
## Mark Cowley, 9 June 2005
##
hist.character <- function(words, x.labels=T, na.rm=T, xlab="", main="Histogram of word counts", xaxt=ifelse(x.labels, "n", "s"), plot=TRUE, ...) {
    if(na.rm & sum(is.na(words)) > 0)
        words <- words[!is.na(words)]

    words <- factor(words, levels=unique(words))
    tmp <- hist(as.numeric(words), breaks=c(0:length(levels(words)))+0.5, xlab=xlab, main=main,
                xaxt=xaxt, plot=plot, ...)

    if( plot & x.labels )
        axis(side=1, at=c(1:(length(levels(words)))), labels=levels(words), tick=T)

    tmp <- tmp$counts
    names(tmp) <- levels(words)

    invisible( tmp )
}
## hist.character(c("a","a","a","b","c"))

## tmp <- hist.character(c("b","a","c","a","c","a","b","b","a")); tmp
## as.numeric(as.factor((c("b","a","c","a","c","a","b","b","a"))))


barplot.character <- function(words, na.rm=T, xlab="", main="Barplot of word counts", ...) {
    if(na.rm & sum(is.na(words)) > 0)
        words <- words[!is.na(words)]

    unique <- unique(words)
    counts <- rep(0, length(words))

    for(i in 1:length(counts))
        counts[i] <- sum(words == unique[i])
    names(counts) <- unique

    barplot(counts, main=main)#, xlab=xlab, ...)## xlim=c(1,length(counts)), ...)
}

## barplot.character(c("a","a","a","b","c"))
## barplot.character(c("b","a","c","a","c","a","b","b","a"))

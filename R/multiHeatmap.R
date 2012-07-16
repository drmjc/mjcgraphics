NULL
# #
# # Mark Robinson's multiHeatmap functions
# #
# 
# # source("multiHeatmap.R")
# # library(gplots)
# # 
# # cL<-NULL
# # br<-seq(-3,3,length=101)
# # col<-colorpanel(low="blue",mid="grey",high="red",n=100)
# # cL[[1]]<-list(breaks=br,colors=col)
# # br<-seq(-2,2,length=101)
# # col<-colorpanel(low="green",mid="black",high="red",n=100)
# # cL[[2]]<-list(breaks=br,colors=col)
# # br<-seq(0,20,length=101)
# # col<-colorpanel(low="black",mid="grey",high="white",n=100)
# # cL[[3]]<-list(breaks=br,colors=col)
# # 
# # testD<-list(matrix(runif(400),nrow=20),matrix(rnorm(100),nrow=20),matrix(rpois(100,lambda=10),nrow=20))
# # colnames(testD[[1]])<-letters[1:20]
# # rownames(testD[[1]])<-paste("row",1:20,sep="")
# # 
# # multiHeatmap(testD,cL,xspace=1)
# # 
# # rownames(testD[[1]])<-letters[1:20]
# # rownames(testD[[3]])<-toupper(letters[1:20])
# # multiHeatmap(testD,cL,xspace=c(4,1,10,1),main="Title")
# # 
# # 
# # multiHeatmap(testD,cL,xspace=c(4,1,10,1),main="Title",titles=c("1st title","2nd title","3rd title"))
# # 
# # 
# 
# makeImageRect <- function(x, breaks, colorscale, name) {
#     nr<-nrow(x)
#     nc<-ncol(x)
#     xx <- (1:nc)/nc
#     yy <- (1:nr)/nr
#     right <- rep(xx, nr)
#     top <- rep(yy, each = nc)
#     cols <- as.character(cut(t(x[nr:1,]),breaks,labels=colorscale,include.lowest=FALSE))
#     rectGrob(x = right, y = top, width = 1/nc, height = 1/nr, just = c("right", "top"), gp = gpar(col = NA, fill = cols), name = name)
# }
# 
# multiHeatmap<-function(dataList,colourList,titles=NULL,main="",xspace=1,cwidth=.5,ystarts=c(.05,.90,.925,.95,.98),rlabelcex=1,clabelcex=1,titlecex=1.2,maincex=1.5,scalecex=.7) {
#   require(grid)
#   if(length(xspace)==1)
#     xspace<-rep(xspace,length(dataList)+1)
#   if(length(colourList)==1) {
#     cL<-vector("list",length(dataList))
#     for(i in 1:length(cL))
#       cL[[i]]<-colourList[[1]]
#     colourList<-cL
#   }
#   if(length(ystarts)!=5)
#     stop("'ystarts' must be length=5")
#   if(length(titles)!=length(colourList) & !is.null(titles))
#     stop("'titles' must be length=length(dataList)")
#   if(length(dataList)!=length(colourList))
#     stop("'colourList' must be length=1 or length(dataList)")
#   if(length(xspace)!=(length(colourList)+1))
#     stop("'xspace' must be length=1 or length(dataList)+1")
#   nc<-sapply(dataList,ncol)
#   ndL<-length(dataList)
#   tw<-sum(nc)+sum(xspace)
#   start<-(c(0,cumsum(nc[-length(nc)]))+cumsum(xspace[-(ndL+1)]))/tw
#   end<-(cumsum(nc)+cumsum(xspace[-(ndL+1)]))/tw
#   grid.newpage()
#   for(i in 1:length(dataList)) {
#     # ---------------- data heatmap
#     vp1<-viewport(x=start[i],y=ystarts[1],w=(end[i]-start[i]),h=diff(ystarts[1:2]),just=c("left","bottom"),name="vp1")
#     if(min(dataList[[i]],na.rm=TRUE)<=colourList[[i]]$breaks[1])
#       colourList[[i]]$breaks[1]<-min(dataList[[i]],na.rm=TRUE)-.1
#     nb<-length(colourList[[i]]$breaks)
#     if(max(dataList[[i]],na.rm=TRUE)>=colourList[[i]]$breaks[nb])
#       colourList[[i]]$breaks[nb]<-max(dataList[[i]],na.rm=TRUE)+.1
#     g<-makeImageRect(dataList[[i]],colourList[[i]]$breaks,colourList[[i]]$colors,name="mark")
#     hM<-gTree(children = gList(g),name="heatMap",vp=vp1)
#     grid.draw(hM)
#     # ---------------- colour scale
#     vp2<-viewport(x=end[i],y=ystarts[3],w=min(cwidth*nc/tw),h=diff(ystarts[3:4]),just=c("right","bottom"),name="vp2")
#     h<-makeImageRect(matrix(colourList[[i]]$breaks,nrow=1),colourList[[i]]$breaks,colourList[[i]]$colors,name="mark")
#     hM<-gTree(children = gList(h),name="heatMap",vp=vp2)
#     grid.draw(hM)
#     # ---------------- colour scale labels
#     p<-c(0,.5,1)
#     q<-round(quantile(cL[[i]]$breaks,p=p))
#     w<-min(cwidth*nc/tw)
#     grid.text(as.character(q),x=end[i]-p[length(p):1]*w,y=median(ystarts[2:3]),gp=gpar(cex=scalecex))
#     # ---------------- column labels
#     w<-(end[i]-start[i])/nc[i]
#     grid.text(colnames(dataList[[i]]),x=seq(start[i]+w/2,end[i]-w/2,length=nc[i]),y=ystarts[1],hjust=1.1,vjust=.5,gp=gpar(cex=clabelcex),rot=60)
#     # ---------------- row labels
#     h<-0.8/nrow(dataList[[i]])
#     grid.text(rownames(dataList[[i]]),x=start[i]-.05*xspace[i]/tw,y=seq(ystarts[2]-h/2,ystarts[1]+h/2,length=nrow(dataList[[i]])),hjust=.95,vjust=.5,gp=gpar(cex=rlabelcex))
#     # ---------------- titles
#     grid.text(titles[[i]],x=median(c(start[i],end[i])),y=median(c(ystarts[4],ystarts[5])),gp=gpar(cex=titlecex))
#   }
#   grid.text(main,x=.5,y=median(ystarts[5],1),gp=gpar(cex=maincex))
# }
# 

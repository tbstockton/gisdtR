interactive_plot.fn = function(data.obj=gsd.df,gsd.version = "v2.009 CaseC",
    response="MaxI129_SectorB_20ky[pCi/L]",
    predictor="Reducingmiddle_concrete_kds_redKd_Dist[I][mL/g]",
    group=NULL,
    ylab="Max I129 SectorB 20ky [pCi/L]",xlab="I Reducing middle concrete Kd [mL/g]",
    legend.columns=3,pd.col="darkblue",
    #response.colors = c("goldenrod","yellowgreen","palegreen","whitesmoke")
    response.colors = c("gray75","gray62","gray50","gray37","gray25")
){
  options(stringsAsFactors=F)
  library("RSVGTipsDevice");library("RSvgDevice");library("Cairo")
  load(file=paste("Rdata_v2/",gsub("\\[.*\\]","",response),"_",gsd.version,"_endpoint.gbm.Rdata",sep=""))
  endpoint.gbm$sa = plot.pd.gbm(endpoint.gbm,gsd.df,nVar=grep(gsub("`|\\[|\\]|/","",predictor),gsub("`|\\[|\\]|/","",summary(endpoint.gbm,plotit=F)$var)),res=1000)
  
  #poly.x = signif(c(range(data.obj[,predictor]),rev(range(data.obj[,predictor]))),poly.x.signif)
  poly.x = c(floor(min(data.obj[,predictor])),ceiling(max(data.obj[,predictor])))
  #poly.x = c(min(pretty(data.obj[,predictor])),max(pretty(data.obj[,predictor])))
  response.quan = quantile(data.obj[,response])
  response.hist = hist(data.obj[,predictor],plot=F)
  response.hist$y = (response.hist$counts)/max(range(response.hist$counts)) * diff(range(data.obj[,response]))+min(data.obj[,response])
  response.hist$at = pretty(seq(min(response.hist$count),max(response.hist$count),length=4)) 
  response.hist$at = (response.hist$at)/max(range(response.hist$at)) * diff(range(data.obj[,response]))+min(data.obj[,response])
  
  group.cols=rainbow(length(unique(data.obj[,group])))
  point.cols = data.frame(group=sort(unique(data.obj[,group])),cols=group.cols)
  data.obj = merge(data.obj,point.cols,by.x=group,by.y="group")
  
  devSVGTips(file="pd.svg",height=8,width=15,toolTipMode = 2, toolTipFontSize = 9)
  #dev.off();windows();browser()
  par(mai=c(3,1.5,1,5.0))
  plot(data.obj[,predictor],data.obj[,response],type="n",las=1,ylab=paste(ylab,"\n"),xlab=xlab)

  axis(side=4,las=2,
       at=response.hist$at,
       labels=pretty(seq(min(response.hist$count),max(response.hist$count),length=4)),
       )
  ylab.histo = gsub("\\[.*\\]","",xlab)
  setSVGShapeToolTip(title="Histogram", desc=ylab.histo)
  mtext(text=gsub("\\[.*\\]","",xlab),side=4,line=4)
  setSVGShapeToolTip(title="Histogram", desc=ylab.histo)
  mtext(text="Count",side=4,line=5.5)
  
#  lapply(1:(length(response.quan)-1),function(i){
#          setSVGShapeToolTip(title="Percentile", desc=c("0-25","25-50","50-75","75-100")[i])
#          rect(min(poly.x),response.quan[i],max(poly.x),response.quan[i+1],col=response.colors[i],border=0)
#        })

  for(i in 1:length(response.hist$y)){
    setSVGShapeToolTip(title=xlab, desc1=paste(response.hist$breaks[i],"to",response.hist$breaks[i+1]),desc2=paste("count=",response.hist$count[i]))
    rect(response.hist$breaks[i],response.quan[1],response.hist$breaks[i+1],response.hist$y[i],col="gray90",border="gray75")
  }
  if(is.null(endpoint.gbm$realization)) endpoint.gbm$realization = 1:length(endpoint.gbm$fit)
  
  # label each point
  for(i in 1:nrow(data.obj)){
    setSVGShapeToolTip(paste("Realization: ",endpoint.gbm$realization[i],sep=""),
        desc1=paste(ylab,": ",signif(data.obj[i,response],3),sep=""),
        desc2=paste(xlab,": ",signif(data.obj[i,predictor],3),sep="")
    )
    points(data.obj[i,predictor],data.obj[i,response],col=data.obj[i,"cols"],pch=16,cex=.5)
  }
  
  for(i in 1:length(response.quan)){
    setSVGShapeToolTip(title=ylab, desc1=paste("Percentile",c("min","25","50","75","max")[i]))
    lines(x=c(min(poly.x),max(poly.x)),c(response.quan[i],response.quan[i]),col=response.colors[i],lty=3,lwd=3)
  }

  lines(endpoint.gbm$sa$x,endpoint.gbm$sa$y,type="l",lwd=4,col=pd.col)

  # legend
  uni.groups = paste(". ",group,": ",sort(unique(data.obj[,group])),sep="")
  setSVGShapeToolTip(title="Realizations", desc1=paste(nrow(data.obj),"Simulated values from the GoldSim model"),desc2=paste("colored by",group))
  mtext(uni.groups,side=1,line=seq(5.0,5+(length(uni.groups)-1)*1.5,by=1.5),adj=0.25,col=group.cols,font=3)
  setSVGShapeToolTip(title="Partial Dependence", desc1=paste("Change in the",ylab),desc2=paste("wrt a change in",xlab))
  mtext("---- Partial Dependence",side=1,line=5,adj=0.75,col=pd.col,font=2)
  setSVGShapeToolTip(title="Percentiles", desc=ylab)
  mtext(paste(ylab,"Percentiles"),side=1,line=6.5,adj=1,col="black",font=2)
  for(i in 1:length(response.quan)){
    setSVGShapeToolTip(title="Quantiles", desc=ylab)
    mtext(c("----- min","----- 25th","----- 50th","----- 75th","----- max")[i],side=1,line=c(8.0,9.5,11,12.5,14.0)[i],adj=0.70,col=response.colors[i],font=2)
  }
  dev.off()
  browseURL(paste("file://",getwd(),"/pd.svg",sep=""),browser="C:/Program Files/Mozilla Firefox/firefox.exe")
}

sa = function(response=NULL,data=NULL,gsd.version = NULL,gsd.dir="gsd",
              run_gbm = "new",n.trees=5000,n.samples=NULL,
              scale_response=F,log_response=F,
              SI_include_level = 5,
              shrinkage=0.001,train.fraction=1,bag.fraction=1,
              interaction.depth = 10,n.minobsinnode = 4,
              n.extreme = 5,plots=T,
              response.quan = T,response.colors = c("goldenrod","lightgreen","lightblue","lightpink"),
              density.cex=0.7,pd.cex=0.7,quants.probs=c(0.5,0.95,0.99),
              subset_ind=NULL,endpoints.id=NULL,extremes=FALSE,
              remove.fields=NULL,new.fields=NULL,Rdata.name=NULL,label.prefix="",
              bright_line=NULL){
  #' Sensitivity Analysis using Machine Learning
  #' @param response .
  #' @param data .
  #' @param gsd.version .
  #' @param gsd.dir .
  #' @return data.
  #' @export
  options(stringsAsFactors=F)
  library(gbm); library(grid); library(XLConnect); #library(xlsx)

  # create a base name for plots and the gbm model
  gsd.version.file = gsub("__","_",gsub("\\.\\*| ","_",paste(gsd.version,collapse="_")))
  if(run_gbm=="new"){
  	  if(is.null(data)){
        # read in gsd(z) files
        if(length(gsd.version)>0) gsd.version = paste(gsd.version,collapse=".*")
        gsd.df = read.gsd(gsd.dir = gsd.dir,gsd.version = gsd.version)
      }else{
        gsd.df = data
      }
    # subsample the input data is n.samples is specified
    if(is.null(n.samples)){
      ind = 1:nrow(gsd.df)
    }else{
      ind = sample(1:nrow(gsd.df),n.samples,replace=F)
    }
    gsd.df = gsd.df[ind,]
    if(!is.null(subset_ind)){
      gsd.df = gsd.df[subset_ind,]
    }

    if(is.null(endpoints.id)){
      endpoints = response
    }else{
      if(!is.numeric(endpoints.id)){
        endpoints = names(gsd.df)[grep(paste(endpoints.id,collapse="|"),names(gsd.df))]
      }else{
        endpoints = names(gsd.df)[endpoints.id]
      }
    }
    if(is.null(response)){
      response = select.list(endpoints)
    }else if(is.numeric(response)){
	    response.col = response
      response = endpoints[response]
    }
    cat(response,"\n")
    # response_units = paste(label.prefix,gsub("\\]","",unlist(strsplit(response,"\\["))[2]),"\n")
    # # guess at units
	# gsd.units = data.frame(units=as.character(gsub("^.*\\[","\\[",names(gsd.df))),row.names=names(gsd.df))

    if(!is.null(new.fields)) eval(parse(text=new.fields))
    # removes fields based on remove.fields argument
    if(!is.null(remove.fields)) gsd.df = gsd.df[,!(names(gsd.df) %in% remove.fields)]
    # remove endpoints not being modeling during this run
    gsd.df = gsd.df[,names(gsd.df)[!(names(gsd.df) %in%  endpoints[!(endpoints %in% response)])]]
    names(gsd.df)[1] = "response"

    if(scale_response=="mean") {
      gsd.df$response = scale(gsd.df$response,T,F)
    }else if(scale_response=="meanvar") {
      gsd.df$response = scale(gsd.df$response,T,T)
    }else if(log_response) {
      gsd.df$response[gsd.df$response==0] = 0.0000001
      gsd.df$response = log10(gsd.df$response)
    }
    # get rid of fields with no variation, turn fields into factors
    for(i.fld in ncol(gsd.df):1){
      #i.fld = ncol(gsd.df)
      n.uni = length(unique(gsd.df[,i.fld]))
      is.discrete = grepl("Number",names(gsd.df)[i.fld])
      if(n.uni==1){
        gsd.df = gsd.df[,-i.fld]
      }else if(n.uni<6 | is.discrete){
        gsd.df[,i.fld] = as.factor(gsd.df[,i.fld])
      }
    }

    if(is.null(Rdata.name))
      Rdata.name = file.path("Rdata",gsub(" |/","",paste(gsub("\\[.*\\]","",response),"",gsd.version.file,"_endpoint.gbm.Rdata",sep="")))
    cat("running gbm ... ")
    if(train.fraction==1){
      endpoint.gbm = gbm(response~.,data=gsd.df,keep.data=T,verbose=T,distribution="gaussian",n.trees=n.trees,interaction.depth = interaction.depth,n.minobsinnode = n.minobsinnode)
    } else {
      endpoint.gbm = gbm.fit(y=gsd.df$response,x=gsd.df[,-1],keep.data=T,verbose=T,distribution="gaussian",shrinkage=shrinkage,
        n.trees=n.trees,interaction.depth=interaction.depth,
        n.minobsinnode = n.minobsinnode,train.fraction=train.fraction,bag.fraction=bag.fraction)
        n.tree.opt = min(n.trees,1.2*gbm.perf(endpoint.gbm,method="OOB",plot.it=T))
        endpoint.gbm$fit = predict.gbm(endpoint.gbm,newdata=gsd.df[,-1],n.trees=n.tree.opt)
      print(paste("Optimal # of trees is",n.tree.opt))
    }
    endpoint.gbm$response = gsd.df$response
    endpoint.gbm$response.name = response
    endpoint.gbm$log_response = log_response
    endpoint.gbm$realization = ind
    save(endpoint.gbm,file=Rdata.name)
    cat("done\n")
  } else { # run_gbm=="new"
    if(is.null(Rdata.name)) Rdata.name = gsub(" |/","",file.path("Rdata",paste(gsub("\\[.*\\]","",response),"",gsd.version.file,"_endpoint.gbm.Rdata",sep="")))
    load(file=Rdata.name)
    if(run_gbm=="more"){
      #endpoint.gbm$data = gsd.df
      endpoint.gbm = gbm.more(endpoint.gbm,n.new.trees=n.trees,verbose=T)
      #endpoint.gbm$response = gsd.df$response
      endpoint.gbm$response.name = response
      endpoint.gbm$realization = 1:length(endpoint.gbm$response.name)
      save(endpoint.gbm,file=Rdata.name)
    }
  }

  cat("orig response:",endpoint.gbm$response.name,"\n")
  response = massage.labels(gsd.labels=endpoint.gbm$response.name,labels.table='massage_labels.xlsx')
  response_units = gsub("\\]","",unlist(strsplit(endpoint.gbm$response.name,"\\["))[2])
  if(is.na(response_units)) response_units = "" else response_units = paste(label.prefix,response_units,"\n")
  log_response = endpoint.gbm$log_response
  endpoint.gbm$var.names = massage.labels(endpoint.gbm$var.names,labels.table='massage_labels.xlsx')
  gsd.df = as.data.frame(matrix(endpoint.gbm$data$x,nrow=length(endpoint.gbm$data$y)))
  names(gsd.df) = endpoint.gbm$var.names
  gsd.df$response = endpoint.gbm$data$y
  bright_line.orig = bright_line
  if(log_response) {
    response_orig = 10^gsd.df$response
    if(!is.null(bright_line)) { bright_line = log10(bright_line) }
  } else {
    response_orig = gsd.df$response
  }
  quants = c(Mean=mean(response_orig),quantile(response_orig,probs=quants.probs))

  # guess at units

  if(!is.null(response.quan)) response.quan = quantile(endpoint.gbm$response)

  write.csv(summary(endpoint.gbm,plotit=FALSE),
            paste("summaries/",gsd.version.file,"_",
                  gsub("\\[.*\\]|\\(.*\\)| |:","",response),
                  "_parameter_si.csv",sep=""),
            row.names=F)

  plot.flags = c(TRUE,rep(FALSE,6))
  plot.list = vector("list",6)
#   i = 1
#   while(plot.flags[i]){
#     endpoint.gbm$sa = plot.pd.gbm(endpoint.gbm,gsd.df,nVar=i,res=1000)
#     if(unique(endpoint.gbm$sa$SI) > SI_include_level){
#       plot.list[[i+1]] = sa.pd.plot(endpoint.gbm,gsd.df=gsd.df,y.label="",nVar=i,layout=c(1,1),loessTF=F,response.colors=response.colors,response.quan=response.quan,pd.cex=pd.cex,log_response=log_response,plotIt=FALSE)
#     }
#     plot.flags[i+1]= unique(endpoint.gbm$sa$SI) > SI_include_level
#     i = i + 1
#   }
  for(i in 1:5){

    endpoint.gbm$sa = plot.pd.gbm(endpoint.gbm,gsd.df,nVar=i,res=1000)
    if(unique(endpoint.gbm$sa$SI) > SI_include_level){
      plot.list[[i+1]] = sa.pd.plot(endpoint.gbm,gsd.df=gsd.df,y.label="",nVar=i,layout=c(1,1),loessTF=F,response.colors=response.colors,response.quan=response.quan,pd.cex=pd.cex,log_response=log_response,plotIt=FALSE)
    }
    plot.flags[i+1]= unique(endpoint.gbm$sa$SI) > SI_include_level
  }

  if(!is.null(bright_line)) violate = length(gsd.df$response[gsd.df$response>bright_line])

  rsquare = 1-endpoint.gbm$train.error[length(endpoint.gbm$train.error)]/endpoint.gbm$train.error[1]
  #gsd.df$response.name = paste(ifelse(nchar(label.prefix)>0,paste(label.prefix," ",sep=""),""),response,"\nR= = ",round(rsquare,2),sep="")
  gsd.df$response.name = paste(
      #ifelse(nchar(label.prefix)>0,paste(label.prefix," ",sep=""),""),
      response
#      ,"\n",expression(R^2)," = ",round(rsquare,2)
      ,"\nR2 = ",round(rsquare,2)
      ,ifelse(is.null(bright_line),"",paste("   Percent > ",bright_line.orig," = ",violate/nrow(gsd.df)*100,"%",sep=""))
      ,paste("   ",names(quants)[1],": ",signif(quants[1],digits=2),collapse="   ",sep="")
      ,"\n",paste(names(quants)[-1],": ",signif(quants[-1],digits=2),collapse="   ",sep="")
#      ,"\n",paste(names(quants)[-c(1,2)],": ",format(signif(quants[-c(1,2)],2),scientific=any(quants<0.000001)),collapse="   ",sep="")
      ,sep="")

  if(extremes){
	  # n.extreme = 10
	  rel.inf = na.omit(summary(endpoint.gbm,plotit=F))
	  gsd.df_response = gsd.df[,"response"]
	  gsd.df_response_name = names(gsd.df)["response"]
	  ind = gsd.df_response > quantile(gsd.df_response,1-n.extreme/nrow(gsd.df))
	  gsd.df_response_ranks = rank(gsd.df_response)[ind]
	  gsd.df_response = gsd.df_response[ind]

	  tmp1 = gsd.df[ind,-grep("response.name",names(gsd.df))]
	  tmp2 = as.data.frame(apply(gsd.df,2,function(x)scale(rank(x),F,F)))[ind,-grep("response.name",names(gsd.df))]
	  ord = rev(order(gsd.df_response))
	  tmp1 = tmp1[ord,]
	  tmp2 = tmp2[ord,]
	  out = as.data.frame(cbind(realization = rep(row.names(tmp1),length=ncol(tmp1)),
	         rep(gsd.df_response[ord],length=ncol(tmp1)),rep(gsd.df_response_ranks[ord],length=ncol(tmp1)),stack(tmp2)[,c("ind","values")],stack(tmp1)[,c("values")]))
	  names(out) = c("realization",response,"rank","parameter","parameter_rank","parameter_value")
	  out = out[order(out[,2],out[,5],decreasing=T),]
	  out.name = paste(ifelse(nchar(label.prefix)>0,paste(label.prefix," ",sep=""),""),gsub("^_","",gsub("/","_",response),gsd.version.file),".xlsx",sep="")
	  #write.table(out,out.name,sep="\t",row.names=F)
    write.xlsx(out,out.name)
  }
  if(!plots) return()

  these_layout.heights=trellis.par.get("layout.heights")
  these_layout.heights$axis.xlab.padding=0.0

  plot.list[[1]] =
      densityplot(~response|response.name,data=gsd.df,n=512,log=F,
        panel=function(x,...){
           #dens = density(x,from=min(x),to=max(x))#,n=length(xData))
           dens = density(x,n=512)
           panel.grid(v=-1,h=0,lty=1)
           #panel.polygon(dens$x,dens$y,col="#FCD8A1",border="#FCD8A1")
           # x.ind = dens$x > quantile(x)[1] & dens$x <= quantile(x)[2]
           x.ind = dens$x <= quantile(x)[2]
           panel.xyplot(dens$x[x.ind],dens$y[x.ind],lwd=2,col=response.colors[1],type="h")
           x.ind = dens$x > quantile(x)[2] & dens$x <= quantile(x)[3]
           panel.xyplot(dens$x[x.ind],dens$y[x.ind],lwd=2,col=response.colors[2],type="h")
           x.ind = dens$x > quantile(x)[3] & dens$x <= quantile(x)[4]
           panel.xyplot(dens$x[x.ind],dens$y[x.ind],lwd=2,col=response.colors[3],type="h")
           # x.ind = dens$x > quantile(x)[4] & dens$x <= quantile(x)[5]
           x.ind = dens$x > quantile(x)[4] & dens$x <= quantile(x)[5]
           panel.xyplot(dens$x[x.ind],dens$y[x.ind],lwd=2,col=response.colors[4],type="h")
           if(!is.null(bright_line)){
             #cat("bright_line: ",bright_line,"\n")
             panel.lines(x=c(bright_line,bright_line),y=c(min(dens$y),max(dens$y)),col="darkred",lwd=2)
             #panel.abline(v=bright_line,col="darkred",lwd=3)
           }
        },
        par.settings=list(strip.background=list(col="#FCD8A1")
           #,layout.widths = these_layout.widths
           ,layout.heights = these_layout.heights),#,mgp=c(1, 1,0)),#layout.widths=list(between=0.5)),
        par.strip.text=list(lines=3.5,lineheight=1,cex=density.cex,col="black",font=1),
        key=list(space="bottom", #corner=c(0.5,0.01),
            between.columns=0.5,between=0.5,
            columns=4,text=list(c("0-25%","25-50%","50-75%","75-100%"),cex=0.5),
            rectangles=list(size=1,col=response.colors,border=response.colors)),
        xlab=list(response_units,cex=density.cex),#list(paste("log",unique(as.character(gsd.units[response,"units"]))),cex=density.cex),
        ylab=list("Probability Density",cex=density.cex),
        scale=list(x=list(relation="free",log=F,cex=density.cex),y=list(relation="free",rot=0,draw=F)),
        xscale.components = function(...) {
          ans <- xscale.components.default(...)
          if(log_response)
            ans$bottom$labels$labels <- 10^ans$bottom$labels$at
          ans
        }
    )

  dev.off()

  # create figures folder
  figs.dir = file.path("figures",gsd.version.file)
  dir.create(figs.dir,recursive=T)

  #fig.name = paste(ifelse(nchar(label.prefix)>0,paste(label.prefix," ",sep=""),""),gsub("^_","",gsub("/","_",response),gsd.version.file),sep="")
  fig.name = gsub("__","_",paste(ifelse(nchar(label.prefix)>0,paste(label.prefix," ",sep=""),""),gsub("^_","",gsub("/|:| ","_",response)),sep=""))
  cat("new response:",response," ...... figure name:",fig.name,"\n")

  png(file=paste(figs.dir,"/",gsd.version.file,"_",fig.name,".png",sep=""),width=6.5,height=8,units="in",res=108)
#  pdf(file=paste(figs.dir,"/",gsd.version.file,"_",fig.name,".pdf",sep=""),width=6.5,height=8)#,units="in")#,res=108)
  grid.rows = ifelse(sum(plot.flags)>4,3,2)
  pushViewport(viewport(layout = grid.layout(grid.rows,2, heights = unit(c(8/grid.rows,8/grid.rows),"inches"),widths = unit(c(3.25,3.25),"inches"))))
  for(i.plot in 1:sum(plot.flags)){
    if(plot.flags[i.plot]) {
      pushViewport(viewport(layout.pos.row = c(1,1,2,2,3,3)[i.plot],layout.pos.col = c(1,2,1,2,1,2)[i.plot]))
      print(plot.list[[i.plot]], newpage = FALSE)
    }
    popViewport()
  }
  dev.off()

  if(!is.null(endpoint.gbm$realization)){
    png(file=paste(figs.dir,"/",gsd.version.file,"_",fig.name,"_fit",".png",sep=""),width=6.5,height=8,units="in",res=108)
#    pdf(file=paste(figs.dir,"/",gsd.version.file,"_",fig.name,"_fit",".pdf",sep=""),width=6.5,height=8)#,units="in",res=108)
    plot(gsd.df$response~endpoint.gbm$fit,ylab=paste(gsd.version,response),xlab=paste("gbm",response))
    abline(0,1,col="darkred")
    dev.off()
  }
  rm(gsd.df,endpoint.gbm)
}

plot.pd.gbm = function(object,saData=NULL,nVar=1,nthVar=NULL,res=100,rescale=F){
  require(gbm)
  rms = function(x){
    n = nrow(x)
    return(sqrt(sum(x^2)/(n-1)))
  }
  if(!is.null(nthVar)){
    rel.inf = na.omit(summary(object,plotit=F)[nthVar,])
  }else{
    rel.inf = na.omit(summary(object,plotit=F)[nVar,])
  }
  nVar = nrow(rel.inf)
  var_name = as.character(rel.inf[,1])
  plotData = NULL
  for(i in 1:nVar){
    tmp = plot.gbm(object,var_name[i],continuous.resolution = res,return.grid=T)
    names(tmp)[1] = "x"
    tmp$SI = rel.inf[i,2]
    tmp$var.name = as.character(rel.inf[i,1])
    tmp$label = paste(as.character(rel.inf[i,1]),"\n SI = ",signif(rel.inf[i,2],3),sep="")
#    if(rescale){
#      tmp$x=rms(saData[which(names(saData)==as.character(rel.inf$var[i]))])*tmp$x
#    }
    plotData = rbind(plotData,tmp)
  }
  plotData
}

sa.pd.plot = function(plotModel,plotIt=TRUE,nVar=4,nthVar=NULL,y.label=NULL,layout=c(2,2),gsd.df=NULL,
    loessTF=T,all.x.numeric=F,y.scale="free",x.scale="free",response.colors=NULL,response.quan=NULL,pd.cex=1,
    log_response=F,...){
  require(Hmisc)
  if(is.null(plotModel$sa)){
    plotModel$sa = plot.pd.gbm(plotModel,...)
  }
  if(is.null(y.label)) y.label = "partial dependence"
  uniqueSIs = rev(sort(unique(plotModel$sa$SI)))
  if(is.null(nthVar)){
    plotModel$sa = subset(plotModel$sa,SI>=uniqueSIs[min(length(uniqueSIs),nVar)])
  }else{
    plotModel$sa = subset(plotModel$sa,SI==uniqueSIs[min(length(uniqueSIs),nthVar)][1])
  }
  plotModel$sa$label = factor(plotModel$sa$label,ordered=T,levels=unique(plotModel$sa$label))
  if(!is.factor(plotModel$sa$x) & (all.x.numeric || !any(is.na(as.numeric(plotModel$sa$x))))){
    plotModel$sa$x = as.numeric(plotModel$sa$x)
    cat("converted x to numeric\n")
  }else if(!any(is.na(as.numeric(as.character(plotModel$sa$x))))){
    plotModel$sa$x = factor(as.numeric(as.character(plotModel$sa$x)),ordered=T)
  }else{
    plotModel$sa$x = as.factor(as.character(plotModel$sa$x))
  }

  plotModel$var.names = gsub("`","",plotModel$var.names)
  plotModel$sa$var.name = gsub("`","",plotModel$sa$var.name)
  plotModel$sa$label = massage.labels(plotModel$sa$label,labels.table='massage_labels.xlsx')

  print(unique(as.character(plotModel$sa$label)))
  these_layout.widths=trellis.par.get("layout.widths")
  these_layout.widths$axis.right=0
  these_layout.widths$right.padding=0
  these_layout.widths$axis.left=0
  these_layout.widths$left.padding=0
  these_layout.widths$ylab.axis.padding=0
  these_layout.widths$xlab.axis.padding=0
  these_layout.widths$ylab=0
  these_layout.widths$layout.widths$between=0
  these_layout.heights=trellis.par.get("layout.heights")
  these_layout.heights$axis.bottom=0
  these_layout.heights$bottom.padding=0
  these_layout.heights$top.padding=0
  these_layout.heights$axis.xlab.padding=0
  these_layout.heights$xlab=0
  these_layout.heights$axis.top=0
  these_layout.heights$main=0
  these_layout.heights$sub=0
  these_layout.heights$between=0
  #trellis.par.set("layout.widths",layout.widths)

#  lab = eval(parse(text=paste("expression(",unique(plotModel$sa$var.name),")")))
#  lab = ifelse(grepl("expression",lab),eval(parse(text=lab)),lab)

   saPlot =
      xyplot(y~x|label,groups=var.name,data=plotModel$sa,
          panel=function(x,y,groups,subscripts,...){
            if(!is.null(response.quan)){
              poly.x = c(current.panel.limits()$xlim,rev(current.panel.limits()$xlim))
              panel.polygon(poly.x,response.quan[c(1,1,2,2)],col=response.colors[1],border=0)
              panel.polygon(poly.x,response.quan[c(2,2,3,3)],col=response.colors[2],border=0)
              panel.polygon(poly.x,response.quan[c(3,3,4,4)],col=response.colors[3],border=0)
              panel.polygon(poly.x,response.quan[c(4,4,5,5)],col=response.colors[4],border=0)
            }
            if(length(unique(x)) > 5 & ! is.factor(x)){
              if(!is.null(gsd.df)){
                xData = na.omit(gsd.df[,unique(groups)[panel.number()]])
                dens = density(xData,from=min(xData),to=max(xData))#,n=length(xData))
                dens$y = (dens$y-min(dens$y))/diff(range(dens$y)) * diff(range(y))+min(y)
                dens$y[1]=min(dens$y)
                dens$y[length(dens$y)]=min(dens$y)
                panel.grid(v=-1,h=0,lty=1)
                #panel.polygon(dens$x,dens$y,col="#E1EBE3FF",border="#E1EBE3FF")
                panel.polygon(dens$x,dens$y,col="#FFFAFA50",border="black",transparent=T)
                #panel.bwplot(x=rep(median(dens$x),length(y)),y,...)
                #panel.abline(h=quantile(y,c(0.1,0.25,0.5,0.75,0.9)),col=rep("darkred",5))
              }
              if(loessTF){
                loessit = try(panel.loess(as.numeric(x),as.numeric(y),type="l",col="darkblue",lw=3,...))
                if(inherits(loessit,"try-error"))
                  panel.xyplot(as.numeric(x),as.numeric(y),type="l",col="darkblue",lw=3,...)
              } else {
                panel.xyplot(as.numeric(x),as.numeric(y),type="l",col="darkblue",lw=3,...)
              }
            } else {
              xData = na.omit(gsd.df[,unique(groups)[panel.number()]])
              xData = table(xData)/length(xData)
              y.plot = min(current.panel.limits()$ylim) + diff(current.panel.limits()$ylim) * xData
              #y.plot = xData * (max(as.numeric(y))/max(xData))
              for(i in 1:length(x)){
                llines(x=c(x[i],x[i]),y=c(min(y),y.plot[i]),col="#FFFAFA80",lw=20)
                llines(x=c(x[i],x[i]),y=c(min(y),y[i]),col="darkblue",lw=5)
              }
#              panel.xyplot(x,y.plot,type="h",col="red",lw=20,...) #col="#FFFAFA80"
#              panel.xyplot(x,as.numeric(y),type="p",col="darkblue",lw=5,...)
            }
          },
          par.settings=list(strip.background=list(col="#E1EBE3FF")
              ,layout.widths = these_layout.widths,layout.heights = these_layout.heights
          ),
         # strip =  function(...,factor.levels) strip.default(...,factor.levels=lab),#,var.name=expression(label)),
          par.strip.text=list(lines=3,cex=pd.cex),
          xlab="",ylab=y.label,between=list(x=1,y=1),
          scale=list(x=list(relation=x.scale,cex=pd.cex),y=list(relation=y.scale,rot=0,cex=pd.cex)),
          yscale.components = function(...) {
            ans <- yscale.components.default(...)
            if(log_response){
              ans$left$labels$labels <- format(10^ans$left$labels$at,digits=2)
              # ans$left$labels$labels <- format(log_response^ans$left$labels$at,digits=2)
            } else {
              ans$left$labels$labels <- format(ans$left$labels$at,scientific=TRUE,digits=2)
            }
            ans
          },
          layout=layout,
          as.table=T)

        xscale.components = function(...) {
          ans <- xscale.components.default(...)
          if(log_response)
            ans$bottom$labels$labels <- 10^ans$bottom$labels$at
          ans
        }

  if(plotIt)  print(saPlot)
  return(saPlot)
}

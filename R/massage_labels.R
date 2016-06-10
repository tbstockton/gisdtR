massage.labels = function(gsd.labels,labels.table="massage_labels.txt",final.cleanup=TRUE){

    # Read in an Excel speadsheet with a gsd_label column that indicates the label to change
  # to the pretty_label column
  # gsd.labels = names(gsd.df)
  gsd.labels =  as.character(gsd.labels)
  if(grepl("xls",labels.table)){
#     library(xlsx)
#     massage.labels.df = read.xlsx("massage_labels.xlsx",sheetIndex=1)
    library(XLConnect)
    massage_labels.wb = loadWorkbook("massage_labels.xlsx")
    massage_labels.df = readWorksheet(massage_labels.wb,1)
    massage.labels.df$gsd_label[is.na(massage.labels.df$gsd_label)]=""
    massage.labels.df$pretty_label[is.na(massage.labels.df$pretty_label)]=""
    massage.labels.df$type[is.na(massage.labels.df$type)]="gsub"
  }else{
    massage.labels.df = read.table("massage_labels.txt",as.is=T,sep="\t",header=T,stringsAsFactors=F,allowEscapes=T)
  }
  
  multiline = length(grep("\n",gsd.labels))!=0
  if(multiline){
    line2 = do.call("rbind",strsplit(gsd.labels,"\n"))[,2]
    gsd.labels = do.call("rbind",strsplit(gsd.labels,"\n"))[,1]
  }

  for(i in 1:nrow(massage.labels.df)){
    if(massage.labels.df$type[i]=="replace"){
      labels.ind = regexpr(massage.labels.df$gsd_label[i],gsd.labels)>0
      gsd.labels[labels.ind] = massage.labels.df$pretty_label[i]
    }else if(massage.labels.df$type[i]=="gsub"){
      # i = 1
      gsd.labels = try(gsub(massage.labels.df$gsd_label[i],massage.labels.df$pretty_label[i],gsd.labels,fixed=TRUE))
      if(inherits(gsd.labels,"try-error")) browser()
    }
  }

  if(final.cleanup){
#    gsd.labels = gsub("([[:upper:]])"," \\1",gsd.labels,perl=TRUE)
    gsd.labels = gsub("\\["," \\[",gsd.labels)
    gsd.labels = gsub("\\\\","/",gsd.labels)
    gsd.labels = gsub("//","/",gsd.labels)
    gsd.labels = gsub("`","",gsd.labels)
    gsd.labels = gsub("^ ","",gsd.labels)
    gsd.labels = gsub("  "," ",gsd.labels)
  }
  if(multiline){
    gsd.labels = paste(gsd.labels,line2,sep="\n")
  }
  
  return(gsd.labels)
}


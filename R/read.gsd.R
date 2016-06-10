read.gsd = function(gsd.dir="gsd",gsd.version,skip=0) {

  # read in gsd(z) files
  # gsd.version is grep'ed in a list of available files in gsd.dir 
  # so it will read in multiple files
  cat("reading in data from ",gsd.version," ... ")
  if(length(gsd.version)>0) gsd.version = paste(c(gsd.version,"gsz"),collapse=".*")
  gsd.files = list.files(gsd.dir,pattern=gsd.version,full.names=T)
  
#   list.files("gsd","Clive DU PA Model v1.0m.*w3m GN NoDose DeepTime.*gsz")
#   list.files("gsd","Clive DU PA Model v1.0m w03m GN NoDose DeepTime.gsz")
#   grep("Clive DU PA Model v1.0m.*w03m GN NoDose DeepTime.*gsz",list.files("gsd"))
#   ?grep
#   
#   
#   "Clive DU PA Model v1.0m.*w03m GN NoDose DeepTime.*gsz"
#   "Clive DU PA Model v1.0m w3m GN NoDose DeepTime t100ky r1000 s2.gsz""
  
  gsd.df = lapply(1:length(gsd.files),function(i,gsd.files){
    fil = gsd.files[i] 
    cat("\n loading ...",fil)
    unzip(fil,overwrite=T,exdir=gsd.dir)
    # unzip removes the '_' in file names
    fil = list.files(gsd.dir,pattern="gsd",full.names=T)
    # GS v11 doesn't skip first line  skip = 0 previous veriosn need skip = 1
    fil.df = read.delim(gsub("gsz","gsd",fil),skip=skip,check.names=F)[,-1]
    if(inherits(fil.df,"try-error")){
      fil.df = try(read.delim(gsub("_"," ",gsub("gsz","gsd",fil)),skip=1,check.names=F)[,-1])
      unlink(gsub("_"," ",gsub("gsz","gsd",fil)))
    }else{
      unlink(gsub("gsz","gsd",fil))
    }
    fil.df$scenario = gsub(" ","",fil)
    fil.df
  },gsd.files=gsd.files)
# gsd.df = fil.df
  gsd.df = do.call("rbind",gsd.df)
  names(gsd.df) = gsub("\\\\","/",names(gsd.df))
  cat("... done \n")
  return(gsd.df)
}

bayesnet = function(nodes){
  #' Solve Bayesian Network
  #'
  #' @param nodes Bayesian Network with Conditional Probability tables (JSON).
  #' @return Bayesian Network with posterior distributions.
  #' @export
  options(stringsAsFactors=FALSE)
  library(gRain);
  if(class(nodes) == "character"){
    dag.nodes = as.data.frame(do.call("rbind",rjson::fromJSON(nodes)));
  }else{
    dag.nodes = as.data.frame(do.call("rbind",nodes));
  }
  cpt.list = vector("list",nrow(dag.nodes))
  names(cpt.list) = as.character(dag.nodes$id)
  evidence.list = vector("list",nrow(dag.nodes))
  names(evidence.list) = as.character(dag.nodes$id)
  for(i in 1:nrow(dag.nodes)){  # i = 1
    bbn = dag.nodes$specification[[i]]$bbn
    vpar = as.character(unlist(dag.nodes$id)[match(bbn$vpar,unlist(dag.nodes$id))])
    cpt.list[[i]] = cptable(vpar=vpar,levels=bbn$levels,values=bbn$values)
    if(as.logical(bbn$use_evidence) & length(bbn$levels[bbn$evidence==1])>0){
      evidence.list[[i]] = bbn$levels[bbn$evidence==1] #as.logical(bbn$use_evidence)
    }

    #if(as.logical(bbn$use_evidence)){
    #  evidence.list[[i]] = as.logical(bbn$use_evidence)
    #}
  }
  cpt.compiled = compileCPT(cpt.list)
  thisGrain = compile(grain(cpt.compiled),propagate = TRUE)
  evidence.list = evidence.list[!sapply(evidence.list, is.null)]
  #thisGrain = setEvidence(thisGrain,evidence=evidence.list)
  thisGrain = setEvidence(thisGrain,nodes=names(evidence.list),states=evidence.list)
  posterior = lapply(
    querygrain(thisGrain,as.character(unlist(dag.nodes$id)))
    ,function(node){
      paste('{"level":"',names(node),'","value":',node,"}",sep='',collapse=",")
    })
  json_result = paste('[',paste('{"id":',names(posterior),
                                ',"name":"',dag.nodes$name[match(names(posterior),dag.nodes$id)],'","posterior":[',posterior,"]}",
                                sep="",collapse=","),']',sep="")
  json_result
}

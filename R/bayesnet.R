bayesnet = function(nodes){
  # 'Add together two numbers.
  #
  # '@param nodes A number.
  # '@return The sum of \code{x} and \code{y}.
  # '@examples
  # 'add(1, 1)
  # '@export
  options(stringsAsFactors=FALSE)
  library(gRain);
  
  return(nodes)
  
  dag.nodes = jsonlite::fromJSON(nodes);
  cpt.list = vector("list",nrow(dag.nodes))
  names(cpt.list) = dag.nodes$name
  evidence.list = vector("list",nrow(dag.nodes))
  names(evidence.list) = dag.nodes$name
  for(i in 1:nrow(dag.nodes)){  # i = 1
    bbn = rjson::fromJSON(dag.nodes$specification[i])$bbn
    vpar = dag.nodes$name[match(bbn$vpar,dag.nodes$id)]
    cpt.list[[i]] = cptable(vpar=vpar,levels=bbn$levels,values=bbn$values)
    if(bbn$use_evidence){
      evidence.list[[i]] = bbn$evidence  
    }
  }
  cpt.compiled = compileCPT(cpt.list)
  thisGrain = compile(grain(cpt.compiled),propagate = TRUE)
  evidence.list = evidence.list[!sapply(evidence.list, is.null)]
  thisGrain = setEvidence(thisGrain,nslist=evidence.list)
  posterior = lapply(querygrain(thisGrain,dag.nodes$name),function(node){
    paste('{"level":"',names(node),'","value":',node,"}",sep='',collapse=",")
  })
  json_result = paste('[',paste('{"id":',dag.nodes$id[match(names(posterior),dag.nodes$name)],
                                ',"name":"',names(posterior),'","posterior":[',posterior,"]}",
                                sep="",collapse=","),']',sep="")
}


nodes = '[
{"id":53,"name":"Subsidize shade grown coffee","type":"option","specification":[{"type":"option","rows":[{"id":62,"name":"Status Quo","level":"0"},{"id":68,"name":"Selective Implementation","level":"100"},{"id":76,"name":"Dredge","level":"0"}]}]},
{"id":56,"name":"Power Generation","type":"measure","specification":{"rows":[{"level":10000,"value":0},{"level":11250,"value":0.25},{"level":12500,"value":0.5},{"level":13750,"value":0.75},{"level":15000,"value":1}],"equation":"(Reservoir_Volume*650)-3000","type":"continuous","bbn":{"levels":["10000.00 to 11250.00","11250.00 to 12500.00","12500.00 to 13750.00","13750.00 to 15000.00"],"vpar":[56,73],"values":[0.4,0.3,0.2,0.1,0,0.05,0.2,0.85],"prob":[0.25,0.25,0.25,0.25],"evidence":[0.25,0.25,0.25,0.25],"use_evidence":"false"}}},
{"id":58,"name":"Coral cover","type":"measure","specification":{"rows":[{"level":0,"value":0},{"level":250,"value":0.25},{"level":500,"value":0.5},{"level":750,"value":0.75},{"level":1000,"value":1}],"equation":"exp(3.17 - 0.017 * Reef_Sedimentation/35.5)","type":"continuous","trigger":{"checked":true,"date":"09/22/2014","level":500},"data":{"schema":"watershed","table":"node_data","fields":["date","coral_cover"]},"bbn":{"levels":["0.00 to 250.00","250.00 to 500.00","500.00 to 750.00","750.00 to 1000.00"],"vpar":[58,75,74],"values":[0.1,0.1,0.2,0.6,0.3,0.35,0.25,0.1,0.25,0.25,0.25,0.25,0.65,0.15,0.15,0.05],"prob":[0.25,0.25,0.25,0.25],"evidence":[0.25,0.25,0.25,0.25],"use_evidence":"false"}}},{"id":59,"name":"Dredge reservoir","type":"option","specification":[{"type":"option","rows":[{"id":62,"name":"Status Quo","level":"0"},
{"id":68,"name":"Selective Implementation","level":"0"},
{"id":76,"name":"Dredge","level":"4000000"}]}]},
{"id":71,"name":"Precipitation","type":"state","specification":{"type":"distribution","name":"normal","parameters":{"mean":"550","std":"50"},"bbn":{"levels":["Low","High"],"prob":[0.75,0.25],"vpar":[71],"values":[0.75,0.25],"evidence":[0.5,0.5],"use_evidence":"false"}}},
{"id":72,"name":"Catchment Sediment Trapping","type":"state","specification":{"type":"distribution","name":"normal","parameters":{"mean":".9","std":".025"},"bbn":{"levels":["Low","High"],"prob":[0.5,0.5],"vpar":[72],"values":[0.5,0.5],"evidence":[0.5,0.5],"use_evidence":"false"}}},
{"id":73,"name":"Reservoir Volume","type":"state","specification":{"type":"equation","equation":"20.35-(Sediment_Yield/1000000)*Catchment_Sediment_Trapping+(Dredge_reservior/1000000)","bbn":{"levels":["Low","High"],"vpar":[73,72,59,74],"values":[0.5,0.5,0.45,0.55,0.5,0.5,0.05,0.95,0.95,0.05,0.85,0.15,0.85,0.15,0.1,0.9],"prob":[0.5,0.5],"evidence":[0.5,0.5],"use_evidence":"false"},"trigger":{"checked":true,"date":"06/05/2015","level":50},"data":{"schema":"watershed","table":"node_data","fields":["date","reservoir_volume"]}}},
{"id":74,"name":"Sediment Yield","type":"state","specification":{"type":"equation","equation":"((Precipitation*40*0.1*50 /1000)*Subsidize_shade_grown_coffee+(Precipitation*40*0.1*125/1000)*(1000-Subsidize_shade_grown_coffee)+(Precipitation*40*0.1*40/1000)*3000)*0.007","trigger":{"checked":true,"date":"12/04/2014","level":44.5},"data":{"schema":"watershed","table":"node_data","fields":["date","sediment_yield"]},"bbn":{"vpar":[74,53,71],"levels":["Low","High"],"values":[0.5,0.5,0.85,0.15,0.1,0.9,0.6,0.4],"prob":[0.5,0.5],"evidence":[0.5,0.5],"use_evidence":"false"}}},
{"id":75,"name":"Reef Sedimentation","type":"state","specification":{"type":"equation","equation":"Sediment_Yield*(1-Catchment_Sediment_Trapping)","bbn":{"levels":["Low","High"],"vpar":[75,72,74],"values":[0.5,0.5,0.85,0.15,0.15,0.85,0.65,0.35],"prob":[0.5,0.5],"evidence":[0.5,0.5],"use_evidence":"false"}}}
]'

nodes = '[{"id":53,"name":"Subsidize shade grown coffee","type":"option","specification":"[{\"type\":\"option\",\"rows\":[{\"id\":62,\"name\":\"Status Quo\",\"level\":\"0\"},{\"id\":68,\"name\":\"Selective Implementation\",\"level\":\"100\"},{\"id\":76,\"name\":\"Dredge\",\"level\":\"0\"}]}]"},{"id":56,"name":"Power Generation","type":"measure","specification":"{\"rows\":[{\"level\":10000,\"value\":0},{\"level\":11250,\"value\":0.25},{\"level\":12500,\"value\":0.5},{\"level\":13750,\"value\":0.75},{\"level\":15000,\"value\":1}],\"equation\":\"(Reservoir_Volume*650)-3000\",\"type\":\"continuous\",\"bbn\":{\"levels\":[\"10000.00 to 11250.00\",\"11250.00 to 12500.00\",\"12500.00 to 13750.00\",\"13750.00 to 15000.00\"],\"vpar\":[56,73],\"values\":[0.4,0.3,0.2,0.1,0,0.05,0.2,0.85],\"prob\":[0.25,0.25,0.25,0.25],\"evidence\":[0.25,0.25,0.25,0.25],\"use_evidence\":\"false\"}}"},{"id":58,"name":"Coral cover","type":"measure","specification":"{\"rows\":[{\"level\":0,\"value\":0},{\"level\":250,\"value\":0.25},{\"level\":500,\"value\":0.5},{\"level\":750,\"value\":0.75},{\"level\":1000,\"value\":1}],\"equation\":\"exp(3.17 - 0.017 * Reef_Sedimentation/35.5)\",\"type\":\"continuous\",\"trigger\":{\"checked\":true,\"date\":\"09/22/2014\",\"level\":500},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"coral_cover\"]},\"bbn\":{\"levels\":[\"0.00 to 250.00\",\"250.00 to 500.00\",\"500.00 to 750.00\",\"750.00 to 1000.00\"],\"vpar\":[58,75,74],\"values\":[0.1,0.1,0.2,0.6,0.3,0.35,0.25,0.1,0.25,0.25,0.25,0.25,0.65,0.15,0.15,0.05],\"prob\":[0.25,0.25,0.25,0.25],\"evidence\":[0.25,0.25,0.25,0.25],\"use_evidence\":\"false\"}}"},{"id":59,"name":"Dredge reservoir","type":"option","specification":"[{\"type\":\"option\",\"rows\":[{\"id\":62,\"name\":\"Status Quo\",\"level\":\"0\"},{\"id\":68,\"name\":\"Selective Implementation\",\"level\":\"0\"},{\"id\":76,\"name\":\"Dredge\",\"level\":\"4000000\"}]}]"},{"id":71,"name":"Precipitation","type":"state","specification":"{\"type\":\"distribution\",\"name\":\"normal\",\"parameters\":{\"mean\":\"550\",\"std\":\"50\"},\"bbn\":{\"levels\":[\"Low\",\"High\"],\"prob\":[0.75,0.25],\"vpar\":[71],\"values\":[0.75,0.25],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":72,"name":"Catchment Sediment Trapping","type":"state","specification":"{\"type\":\"distribution\",\"name\":\"normal\",\"parameters\":{\"mean\":\".9\",\"std\":\".025\"},\"bbn\":{\"levels\":[\"Low\",\"High\"],\"prob\":[0.5,0.5],\"vpar\":[72],\"values\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":73,"name":"Reservoir Volume","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"20.35-(Sediment_Yield/1000000)*Catchment_Sediment_Trapping+(Dredge_reservior/1000000)\",\"bbn\":{\"levels\":[\"Low\",\"High\"],\"vpar\":[73,72,59,74],\"values\":[0.5,0.5,0.45,0.55,0.5,0.5,0.05,0.95,0.95,0.05,0.85,0.15,0.85,0.15,0.1,0.9],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"},\"trigger\":{\"checked\":true,\"date\":\"06/05/2015\",\"level\":50},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"reservoir_volume\"]}}"},{"id":74,"name":"Sediment Yield","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"((Precipitation*40*0.1*50 /1000)*Subsidize_shade_grown_coffee+(Precipitation*40*0.1*125/1000)*(1000-Subsidize_shade_grown_coffee)+(Precipitation*40*0.1*40/1000)*3000)*0.007\",\"trigger\":{\"checked\":true,\"date\":\"12/04/2014\",\"level\":44.5},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"sediment_yield\"]},\"bbn\":{\"vpar\":[74,53,71],\"levels\":[\"Low\",\"High\"],\"values\":[0.5,0.5,0.85,0.15,0.1,0.9,0.6,0.4],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":75,"name":"Reef Sedimentation","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"Sediment_Yield*(1-Catchment_Sediment_Trapping)\",\"bbn\":{\"levels\":[\"Low\",\"High\"],\"vpar\":[75,72,74],\"values\":[0.5,0.5,0.85,0.15,0.15,0.85,0.65,0.35],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"}]'
nodes = '[{"id":53,"name":"Subsidize shade grown coffee","type":"option","specification":"[{\"id\":62,\"name\":\"Status Quo\",\"level\":\"0\"},{\"id\":68,\"name\":\"Selective Implementation\",\"level\":\"100\"},{\"id\":76,\"name\":\"Dredge\",\"level\":\"0\"}]"},{"id":56,"name":"Power Generation","type":"measure","specification":"{\"rows\":[{\"level\":10000,\"value\":0},{\"level\":11250,\"value\":0.25},{\"level\":12500,\"value\":0.5},{\"level\":13750,\"value\":0.75},{\"level\":15000,\"value\":1}],\"equation\":\"(Reservoir_Volume*650)-3000\",\"type\":\"continuous\",\"bbn\":{\"levels\":[\"10000.00 to 11250.00\",\"11250.00 to 12500.00\",\"12500.00 to 13750.00\",\"13750.00 to 15000.00\"],\"vpar\":[56,73],\"values\":[0.4,0.3,0.2,0.1,0,0.05,0.2,0.85],\"prob\":[0.25,0.25,0.25,0.25],\"evidence\":[0.25,0.25,0.25,0.25],\"use_evidence\":\"false\"}}"},{"id":58,"name":"Coral cover","type":"measure","specification":"{\"rows\":[{\"level\":0,\"value\":0},{\"level\":250,\"value\":0.25},{\"level\":500,\"value\":0.5},{\"level\":750,\"value\":0.75},{\"level\":1000,\"value\":1}],\"equation\":\"exp(3.17 - 0.017 * Reef_Sedimentation/35.5)\",\"type\":\"continuous\",\"trigger\":{\"checked\":true,\"date\":\"09/22/2014\",\"level\":500},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"coral_cover\"]},\"bbn\":{\"levels\":[\"0.00 to 250.00\",\"250.00 to 500.00\",\"500.00 to 750.00\",\"750.00 to 1000.00\"],\"vpar\":[58,75,74],\"values\":[0.1,0.1,0.2,0.6,0.3,0.35,0.25,0.1,0.25,0.25,0.25,0.25,0.65,0.15,0.15,0.05],\"prob\":[0.25,0.25,0.25,0.25],\"evidence\":[0.25,0.25,0.25,0.25],\"use_evidence\":\"false\"}}"},{"id":59,"name":"Dredge reservoir","type":"option","specification":"[{\"id\":62,\"name\":\"Status Quo\",\"level\":\"0\"},{\"id\":68,\"name\":\"Selective Implementation\",\"level\":\"0\"},{\"id\":76,\"name\":\"Dredge\",\"level\":\"4000000\"}]"},{"id":71,"name":"Precipitation","type":"state","specification":"{\"type\":\"distribution\",\"name\":\"normal\",\"parameters\":{\"mean\":\"550\",\"std\":\"50\"},\"bbn\":{\"levels\":[\"Low\",\"High\"],\"prob\":[0.75,0.25],\"vpar\":[71],\"values\":[0.75,0.25],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":72,"name":"Catchment Sediment Trapping","type":"state","specification":"{\"type\":\"distribution\",\"name\":\"normal\",\"parameters\":{\"mean\":\".9\",\"std\":\".025\"},\"bbn\":{\"levels\":[\"Low\",\"High\"],\"prob\":[0.5,0.5],\"vpar\":[72],\"values\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":73,"name":"Reservoir Volume","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"20.35-(Sediment_Yield/1000000)*Catchment_Sediment_Trapping+(Dredge_reservior/1000000)\",\"bbn\":{\"levels\":[\"Low\",\"High\"],\"vpar\":[73,72,59,74],\"values\":[0.5,0.5,0.45,0.55,0.5,0.5,0.05,0.95,0.95,0.05,0.85,0.15,0.85,0.15,0.1,0.9],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"},\"trigger\":{\"checked\":true,\"date\":\"06/05/2015\",\"level\":50},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"reservoir_volume\"]}}"},{"id":74,"name":"Sediment Yield","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"((Precipitation*40*0.1*50 /1000)*Subsidize_shade_grown_coffee+(Precipitation*40*0.1*125/1000)*(1000-Subsidize_shade_grown_coffee)+(Precipitation*40*0.1*40/1000)*3000)*0.007\",\"trigger\":{\"checked\":true,\"date\":\"12/04/2014\",\"level\":44.5},\"data\":{\"schema\":\"watershed\",\"table\":\"node_data\",\"fields\":[\"date\",\"sediment_yield\"]},\"bbn\":{\"vpar\":[74,53,71],\"levels\":[\"Low\",\"High\"],\"values\":[0.5,0.5,0.85,0.15,0.1,0.9,0.6,0.4],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"},{"id":75,"name":"Reef Sedimentation","type":"state","specification":"{\"type\":\"equation\",\"equation\":\"Sediment_Yield*(1-Catchment_Sediment_Trapping)\",\"bbn\":{\"levels\":[\"Low\",\"High\"],\"vpar\":[75,72,74],\"values\":[0.5,0.5,0.85,0.15,0.15,0.85,0.65,0.35],\"prob\":[0.5,0.5],\"evidence\":[0.5,0.5],\"use_evidence\":\"false\"}}"}]'

edges = '[{"node_from":"53","node_to":"74","predicate":"{"predicate":"is related"}"},{"node_from":"59","node_to":"73","predicate":"{"predicate":"is related"}"},{"node_from":"71","node_to":"74","predicate":"{"predicate":"is related"}"},{"node_from":"72","node_to":"73","predicate":"{"predicate":"is related"}"},{"node_from":"72","node_to":"75","predicate":"{"predicate":"is related"}"},{"node_from":"73","node_to":"56","predicate":"{"predicate":"is related"}"},{"node_from":"74","node_to":"75","predicate":"{"predicate":"is related"}"},{"node_from":"74","node_to":"73","predicate":"{"predicate":"is related"}"},{"node_from":"74","node_to":"58","predicate":"{"predicate":"is related"}"},{"node_from":"75","node_to":"58","predicate":"{"predicate":"is related"}"}]'
nodes  = RJSONIO::fromJSON(nodes)
edges  = RJSONIO::fromJSON(edges)
library(gRbase)
gRbase::jTree(gRbase::moralize(g))
plot(g)
topoSort(g)

nodes[[1]]$specification


scenarios = '[{"id":64,"name":"Status Quo","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":62,"cost":0,"level":0,"management_id":53},{"id":63,"name":"Status Quo","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":62,"cost":0,"level":0,"management_id":59},{"id":69,"name":"Selective Implementation","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":68,"cost":0,"level":0,"management_id":59},{"id":78,"name":"Dredge","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":76,"cost":0,"level":0,"management_id":53},{"id":77,"name":"Dredge","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":76,"cost":50000000,"level":4000000,"management_id":59},{"id":70,"name":"Selective Implementation","specification":null,"spec_category":null,"spec_type":null,"x":null,"y":null,"description":null,"type":null,"units":null,"scenario_id":68,"cost":3000,"level":100,"management_id":53}]'
scenarios  = RJSONIO::fromJSON(scenarios)

scenarios.details = '[{"id":63,"scenario_name":"Status Quo","option_id":63,"management_id":59,"management_name":"Dredge reservoir","scenario_id":62,"level":0,"description":"Status Quo","units":"cubic meters","cost":0,"x":73,"y":33},{"id":64,"scenario_name":"Status Quo","option_id":64,"management_id":53,"management_name":"Subsidize shade grown coffee","scenario_id":62,"level":0,"description":"Status Quo","units":"ha","cost":0,"x":290,"y":33},{"id":69,"scenario_name":"Selective Implementation","option_id":69,"management_id":59,"management_name":"Dredge reservoir","scenario_id":68,"level":0,"description":"Selective Implementation","units":"cubic meters","cost":0,"x":73,"y":33},{"id":70,"scenario_name":"Selective Implementation","option_id":70,"management_id":53,"management_name":"Subsidize shade grown coffee","scenario_id":68,"level":100,"description":"Selective Implementation","units":"ha","cost":3000,"x":290,"y":33},{"id":77,"scenario_name":"Dredge","option_id":77,"management_id":59,"management_name":"Dredge reservoir","scenario_id":76,"level":4000000,"description":"Dredge","units":"cubic meters","cost":50000000,"x":73,"y":33},{"id":78,"scenario_name":"Dredge","option_id":78,"management_id":53,"management_name":"Subsidize shade grown coffee","scenario_id":76,"level":0,"description":"Dredge","units":"ha","cost":0,"x":290,"y":33}]'
scenarios.details = RJSONIO::fromJSON(scenarios.details)
scenarios.details = as.data.frame(do.call("rbind",scenarios.details))

options.spec = NULL
for(id in unique(unlist(scenarios.details$management_id))){ # id = '53'
  options.json = NULL
  tmp = scenarios.details[scenarios.details$management_id==id,c("id","scenario_name","level")]
  for(i in 1:nrow(tmp)){
    options.json = c(options.json,paste("{'id':",tmp[i,1],",'name':'",tmp[i,2],"','level':",tmp[i,3],"}",sep=""))
  }
  options.spec = rbind(options.spec,data.frame(id=id,specficiation=paste("{'type':'option','rows':",paste(options.json,collapse = ","),"}",sep="")))
}



# mcsim(schema,nodeTable,outputTable)
# Written by Will Barnett and Tom Stockton November 2011.
# Updated 02-13-14
# schema = "watershed"; nodeTable = "model";
# schema = "prj_prj_prj_seed_416_482_495"; nodeTable = "model"; save2db=FALSE;
mcsim <- function(nodes,edges) { #,outputTable,nsim=100,save2db=TRUE){
  # This is a wrapper function that calls the simulcalcs() function, which is the workhorse of the network simulations. 
  options(stringsAsFactors=FALSE)
  library(rjson);library(graph);
  modelTime = pg.spi.exec(paste("select content from ",schema,".context WHERE name='foo'",sep=""))$content
  if(!is.null(modelTime)){
    modelTime = fromJSON(modelTime)[[1]]
    modelTime$timeSeq = seq(modelTime$begin,modelTime$end,by=modelTime$frequency)
    assign("modelTime",modelTime,envir = .GlobalEnv)
  }
  if(is.null(nsim)){assign("nsim",10000,envir = .GlobalEnv)}
  # setup up the time of the model run.
  # read in the model nodes and contruct the DAG
  mode(graph.nodes)
  
  graph.nodes = as.data.frame(do.call("rbind",RJSONIO::fromJSON(nodes))); #pg.spi.exec(paste("select * from ",schema,".",nodeTable,sep=""))
  graph.edges = as.data.frame(do.call("rbind",RJSONIO::fromJSON(edges))); #pg.spi.exec(paste("select * from ",schema,".",nodeTable,"_edges",sep=""))
  #g = ftM2graphNEL(cbind(graph.edges$"from_name",graph.edges$"to_name"))
  g = ftM2graphNEL(cbind(graph.edges$"node_from",graph.edges$"node_to"))
  dev.off();plot(g)
  
  # Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g)); #Rgraphviz::renderGraph(plot.graph("watershed","model"))
  # Delete unused nodes, if applicable.
  #nodes(g) = gsub(" ","_",nodes(g))
  used_nodes <- graph::nodes(g)
  # Node names may have spaces. Replace them with "_" via 'gsub.'
  graph.nodes$name <- gsub(" ","_",graph.nodes$name)
  #ind = match(nodes(g),graph.nodes$name)
  ind = match(nodes(g),graph.nodes$id)
  graph.nodes = graph.nodes[ind,]
  row.names(graph.nodes) = graph.nodes$id #graph.nodes$name
  complete = simulcalcs(g=g,graph.nodes=graph.nodes,schema,nsim=nsim)
  return(complete)
}
simulcalcs <- function(g,graph.nodes,schema,nsim=5){
  # simulcalcs.R
  # Will Barnett and Tom Stockton, November 2011.
  # simulcalcs() will calculate simulations for an entire DAG. It calls 
  # simulnode() for each node in the graph, and returns
  # a storage data frame with the node ids, node names, and simulated values.
  # g = DAG in the graphNEL onject form.
  # graph.nodes = the data frame computed from the SQL query.
  # nsim = number of simulations desired.
  require(rjson)
  probs = seq(0.01,0.99,by=0.01)
  # Figure out the longest ancestry in the graph.
  # anc = apply(gRbase::as.adjMAT(g),2,sum)
  # Find out which nodes are roots; i.e., no parents. And how many of them there are.
  roots = nodes(g)[apply(gRbase::as.adjMAT(g),2,sum) == 0]
  leaves = nodes(g)[apply(gRbase::as.adjMAT(g),1,sum) == 0]
  # Simulate nodes sequentially and store results.
  scenarioNodes = pg.spi.exec(paste("select * from ",schema,".","management_scenarios",sep=""))
  if( is.null(scenarioNodes) || nrow(scenarioNodes) == 0 ) scenarioNodes = data.frame(id=1,name="Base Case")
  
  level.list = vector("list",nrow(graph.nodes))
  names(level.list) = nodes(g)
  
  scenLevel.list = vector("list",length(scenarioNodes$id))
  names(scenLevel.list) = scenarioNodes$name
  
  value.list = vector("list",nrow(graph.nodes[graph.nodes$class %in% c("measure"),]))
  names(value.list) = graph.nodes$name[graph.nodes$class %in% c("measure")]
  
  maut.list = vector("list",nrow(graph.nodes[graph.nodes$class %in% c("measure"),]))
  names(maut.list) = graph.nodes$name[graph.nodes$class %in% c("measure")]
  
  index.list = vector("list",length(scenarioNodes$id))
  names(index.list) = scenarioNodes$name
  
  scenValue.list = vector("list",length(scenarioNodes$id))
  names(scenValue.list) = scenarioNodes$name
  
  scenMaut.list = vector("list",length(scenarioNodes$id))
  names(scenMaut.list) = scenarioNodes$name
  
  scenIndex.list = vector("list",length(scenarioNodes$id))
  names(scenIndex.list) = scenarioNodes$name
  
  stochasticNodes = graph.nodes$name[grepl("distribution",graph.nodes$specification) & 
                                       !grepl("equation|function|time",graph.nodes$specification)]
  nsim = ifelse(length(stochasticNodes)>0,nsim,1)
  # Start loop to iterate through the 'levels' of the network. Each level is a subset of nodes which have 
  # previously-simulated parents (or none at all, in the case of the root nodes).
  level = roots
  nodesNotDone = nodes(g) # The while loop requires nodesNotDone to be > 0.
  nodesDone = NULL
  maut = 0
  
  while( length(nodesNotDone) > 0 ){
    # loop through current set of unsolved root nodes
    for( thisLevel in level ){ #   thisLevel = level[1] 
      cat(thisLevel,"\n")
      nodesDone = c(nodesDone,thisLevel)
      nodesNotDone = nodesNotDone[!grepl(thisLevel,nodesNotDone)]
      #nodeSpec = fromJSON(graph.nodes[thisLevel,"specification"])

      nodeSpec = graph.nodes[thisLevel,"specification"][[1]][[1]]
      nodeSpec$id = unlist(graph.nodes[thisLevel,"id"])
      nodeSpec$class = unlist(ifelse(!is.null(graph.nodes[thisLevel,"class"]),graph.nodes[thisLevel,"class"],graph.nodes[thisLevel,"type"]))
      #thisNodeTable = switch(nodeSpec$class, "index" = "indices", "measure" = "measurable_attributes", "state" = "states","option" = "management_options")
      if( is.null(nodeSpec$type) ) { nodeSpec$type = "missing" }
      for(thisScen in scenarioNodes$name){  # thisScen = scenarioNodes$name[1] 
        scenId = scenarioNodes$id[scenarioNodes$name == thisScen]
        nodeLevels = simulnode(spec=nodeSpec,data=scenLevel.list[[thisScen]],n=nsim,scenId=scenId)
        scenLevel.list[[thisScen]][[thisLevel]] <- nodeLevels
        # calculate the value function for measures
        if( nodeSpec$class == "measure" ){
          thisNodeValueFunction = as.data.frame(do.call("rbind",nodeSpec$rows))
          value.list[[thisLevel]] = signif(try(approx(x=unlist(thisNodeValueFunction$level),y=unlist(thisNodeValueFunction$value),xout=nodeLevels,rule=2)$y),6)
          maut.list[[thisLevel]] = value.list[[thisLevel]] * nodeSpec$weight
          maut = maut + mean(value.list[[thisLevel]]) * nodeSpec$weight
          scenValue.list[[thisScen]] = value.list
          scenMaut.list[[thisScen]] = maut.list
          nodeValues.df = as.data.frame(cbind(prob=probs,do.call("cbind",lapply(scenValue.list,function(scen,thisLevel,probs){quantile(scen[[thisLevel]],probs)},thisLevel,probs))))
          nodeValuesJson = paste(apply(nodeValues.df,1,toJSON),collapse=",")
          nodeValuesJson = paste('{"metaData":{"root":"values","fields":',toJSON(names(nodeValues.df)),'},"values":[',nodeValuesJson,']}')
        } # calculate the value function [end]
      } # thisScen [end]
      
      if( nodeSpec$class == "index" ){
        # values
        for(thisScen in scenarioNodes$name){
          thisIndexList = list(with(scenValue.list[[thisScen]],eval(parse(text=nodeSpec$equation))))
          names(thisIndexList) = thisLevel
          index.list[[thisScen]] = thisIndexList
        }
        nodeValues.df = as.data.frame(cbind(prob=probs,do.call("cbind",lapply(index.list,function(scen,thisLevel,probs){quantile(scen[[thisLevel]],probs)},thisLevel,probs))))
        nodeValuesJson = paste(apply(nodeValues.df,1,toJSON),collapse=",")
        nodeValuesJson = paste('{"metaData":{"root":"values","fields":',toJSON(names(nodeValues.df)),'},"values":[',nodeValuesJson,']}')
      } # index
      
      if( nodeSpec$type == "function" ){
        #         nodeValues.df = as.data.frame(do.call("cbind",lapply(scenLevel.list,function(scen,thisLevel){scen[[thisLevel]]},thisLevel)))
        #         nodeValuesJson = toExtJSON(nodeValues.df)
        #         pg.spi.exec(paste("UPDATE ",schema,".",thisNodeTable," SET modeled_Levels = '",nodeLevelsJson,"' WHERE id = ",nodeSpec$id,sep=""))
      } else if( nodeSpec$class == "measure" | ( nodeSpec$class == "state" & nodeSpec$type == "equation" ) ) {
        nodeLevels.df = as.data.frame(cbind(prob=probs,
                                            do.call("cbind",lapply(scenLevel.list,
                                                                   function(scen,thisLevel,probs){
                                                                     return(quantile(scen[[thisLevel]],probs))
                                                                   },thisLevel,probs))
        ))
        nodeLevelsJson = paste(apply(nodeLevels.df,1,toJSON),collapse=",")
        nodeLevelsJson = paste('{"metaData":{"root":"levels","fields":',toJSON(names(nodeLevels.df)),'},"levels":[',nodeLevelsJson,']}')
      }
    } # thisLevel [end]
    
    # Figure out which nodes are "next" in line. Find all the unique children of the current level,  
    # and see which have parents that have data generated.
    levelKids = unique(gRbase::children(level,g))
    level = levelKids[unlist(lapply(levelKids,function(node,nodesDone){ all((gRbase::parents(node,g)) %in% nodesDone )  }, nodesDone ) )]
  } # while [end]
  
  if( nrow(scenarioNodes) > 1 ) { #"index" %in% graph.nodes$type){
    # index.df = as.data.frame(do.call("rbind",lapply(index.list,function(x){as.data.frame(do.call("cbind",lapply(x,mean)))})))
    index.df = as.data.frame(do.call("rbind",
                                     lapply(scenMaut.list,function(x){
                                       tmp = as.data.frame(do.call("cbind",lapply(x,mean)))
                                       data.frame(Scenario_Comparison=apply(tmp,1,sum))
                                     })))
    measures.df = do.call("rbind",lapply(scenValue.list,function(x){as.data.frame(do.call("cbind",lapply(x,mean)))}))
    measures.df = cbind(name=row.names(measures.df),measures.df)
    measures.df = merge(measures.df,index.df,by="row.names",sort=FALSE)[,-1]
    if(save2db) comparisonHtml(measures.df,schema)
  }
  if(length(stochasticNodes)>0){
    
    if( "option" %in% graph.nodes$type ){
      for(i in 1:length(graph.nodes$name[graph.nodes$class == "option"])){ # i = 1
        thisName = graph.nodes$name[graph.nodes$class == "option"][i]
        thisSet = do.call("rbind",lapply(fromJSON(graph.nodes$specification[graph.nodes$name == thisName])$row,function(x)data.frame(level=x$level,Scenario=x$name)))
        if(i ==1 ) {
          optionsLevels = thisSet
        }else{
          optionsLevels = merge(optionsLevels,thisSet,by="Scenario")
        }
      }
      names(optionsLevels) = c("Scenario",graph.nodes$name[graph.nodes$class == "option"])
      independent.df = merge(independent.df,optionsLevels,sort=F)
    }
    
    #   if( length(index.list) > 1 ){
    #     response.df = as.data.frame(do.call("cbind",index.list))
    #   }else{
    #     response.df = as.data.frame(do.call("cbind",scenLevel.list[[1]][leaves]))
    #   }
    
    #independent.df$Scenario = as.factor(independent.df$Scenario)
    #  response.df = reshape2::melt(response.df,measure.vars=names(response.df))
    #  savoi(response.df[,2],independent.df,schema,graph.nodes)
    
    response.df = as.data.frame(do.call("cbind",lapply(scenMaut.list,function(x){
      apply(as.data.frame(do.call("cbind",x)),1,sum)
    })))
    response.df = reshape2::melt(response.df,measure.vars=names(response.df))
    independent.df = independent.df[,-grep("Scenario",names(independent.df))]
    
    # --- 
    tmp = as.data.frame(do.call("rbind",lapply(scenLevel.list,function(x){
      as.data.frame(do.call("cbind",x))
    })))
    tmp1 = gRain::extractCPT(tmp,g)
    tmp1 = gRain::compileCPT(tmp1)
    tmp1 = gRain::grain(tmp1)
    tmp
    
    tmp3 = discretize(tmp,method="hartemink")
    
    
    library(bnlearn);library(graph)
    tmp2 = gs(tmp,whitelist=
                data.frame(from=gsub(" ","_",graph.edges$from_name),to=gsub(" ","_",graph.edges$to_name))
    )
    plot(tmp2)
    names(tmp)
    nodes(g)
    
    measures.df[1:3,]
    names(cbind(measures.df,independent.df))
    # ---     
    savoi(response.df[,2],independent.df,schema,graph.nodes)
  }
  return(paste('done with',nsim,'simulations'))
}

comparisonHtml = function(data,schema){
  winner = data$name[rank(as.numeric(data[,"Scenario_Comparison"])) == nrow(data)]
  width=650;height=30*nrow(data)*(ncol(data)-1) ;
  chartID = paste("g",round(runif(1,10000,20000),0),sep="")
  chartJSON = extJSChart(y="name",x=names(data)[-1],data=data,xTitle="Value",xMin=0,xMax=1,
                         width=width,height=height,
                         type="bar",stacked="false",legend="{position:'right'}",renderTo=chartID)
  chartJSON = paste("chart0=Ext.create('Ext.chart.Chart',",chartJSON,");",sep="")
  chartHtml = paste(
    "<style>",
    ".sa-value{color:darkred;font-weight:normal;}",
    ".sa-measure{color:#084594;font-weight:normal;}",
    ".w-tip{border-bottom:1px #084594 dotted;}",
    "</style>",
    "<div style='padding:0px 10px; text-align:left;'>",
    "<h2>Scenario Comparison</h2>",
    "<p style='text-align:left;'>",
    "The chart below shows the mean Scenario <span class='sa-measure w-tip' id='value1'>Value</span> for each ",
    "<span class='sa-measure w-tip' id='measure1'>Measure</span> and ",
    "<span class='sa-measure w-tip' id='index1'>Index</span>. The <i>Scenario_Comparison</i> Index is a weighted ",
    "average of the all the Measures Value, weighted by the <span class='sa-measure w-tip' id='weights1'>Objective weights</span>. ",
    "The ",winner," Scenario is the highest valued Scenario based on the <i>Scenario_Comparison</i> Value. ",
    "</p>",
    "<div id='",chartID,"' style='padding:0;margin:0;width:",width,"px;height:",height,"px;'></div>",
    "<p style='font-size:75%;text-align:center'>Click on Legend items to hide or show Scenarios</p>",
    "<div id='objs' style='padding:0;margin:0;'></div>",
    "<script>",
    chartJSON,
    "Ext.create('Ext.tip.ToolTip',{target:'value1',html:'<p><span class=\"sa-measure\">Value</span> is the value assigned to <span class=\"sa-measure\">Measure</span> levels by the value function</p>'});",
    "Ext.create('Ext.tip.ToolTip',{target:'measure1',html:'<p>A <span class=\"sa-measure\">Measure</span> provides an assessment of how well a particlar Objective is being meeting by the Scenarios.</p>'});",
    "Ext.create('Ext.tip.ToolTip',{target:'index1',html:'An <span class=\"sa-measure\">Index</span> is a weighted combination of Measure Values'});",
    "</script>",
    "</div>",
    sep="")
  
  if(is.null(pg.spi.exec(paste("SELECT result FROM ",schema,".","results WHERE name = 'scenario_comparison'",sep=""))$result)){
    pg.spi.exec(paste("INSERT INTO ",schema,".","results"," VALUES (DEFAULT,'scenario_comparison',$_$",chartHtml,"$_$)",sep=""))
  }else{
    pg.spi.exec(paste("UPDATE ",schema,".","results"," SET result = $_$",chartHtml,"$_$ WHERE name = ","'scenario_comparison'",sep=""))
  }
  #return(chartHtml)
}

simulnode <- function(spec,id,parentvec=NULL,data,n,scenId){
  # simulnode.R
  # Will Barnett and Tom Stockton, November 2011.
  # simulnode() will calculate simulations of a specific node, given the parents' information and the node's specification.
  # The returned object is a vector called sim, which is the node's simulated values.
  # Given the information about a node and the node's parents, calculate one realization of the node.
  # spec = specification of the node's characteristics; a fromJSON list with the node's type, parameters, levels, etc.
  # parentvec = numeric vector containing the id's of nodeid's parents, if applicable.
  # dat = (all) the data, including the parents' simulated values, if applicable.
  # n = number of simulated values.
  
  # Match the cases:
  if(spec$class == "measure" | spec$class == "index"){
    sim <- with(as.list(data),eval(parse(text=spec$equation)))
  }else if(spec$type == "option"){
    tempsim = as.data.frame(do.call("rbind",spec$rows))
    tempsim = unlist(tempsim$level[tempsim$id==scenId])
    #    assign(dat[dat[,1]==spec$id,2],tempsim)
    sim <- rep(tempsim,n)
  }else if ( spec$type=="distribution" ){
    if( spec$name == "constant" ){
      sim = rep(eval(parse(text=spec$parameters)),n)
    }else if( spec$name == "timeseries" ){
      #       sim = as.data.frame(do.call("rbind",spec$rows))
      sim = spec$rows
    }else{
      sim <- evalDistJson(spec,fun="random",n=n,decode=F)
    }
  } else if ( spec$type=="equation" | spec$type=="function" ){ 
    sim = with(as.list(data),eval(parse(text=spec$equation)))
  } else if ( spec$type=="data" ){ 
    sim = 0
  }
  return(sim)
}











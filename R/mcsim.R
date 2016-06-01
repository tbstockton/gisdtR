mcsim <- function(nodes,edges,nsim=100){
  #' This is a wrapper function that calls the simulcalcs() function, which is the workhorse of the network simulations.
  #' Read nodes and edges tables from the database and build the DAG
  #' mcsim(schema,nodeTable,outputTable)
  #' Written by Tom Stockton and Will Barnett, November 2011.
  #' Updated 05-22-12, 07-04-2014
  #' mcsim(schema='watershed', nodeTable='model')
  #' @param nodes (JSON).
  #' @param edges (JSON).
  #' @return Bayesian Network with posterior distributions.
  #' @export
  options(stringsAsFactors=FALSE)
  library(rjson);library(graph);
  # nodesJson = nodes; edgesJson = edges;
  graph.nodes = nodes; as.data.frame(do.call("rbind",nodes))
  graph.edges = as.data.frame(do.call("rbind",edges))
  scenarioNodes = as.data.frame(do.call("rbind",scenarios))
#   modelTime = pg.spi.exec(paste("select content from ",schema,".context WHERE name='foo'",sep=""))$content
#   if(!is.null(modelTime)){
#     modelTime = fromJSON(modelTime)[[1]]
#     modelTime$timeSeq = seq(modelTime$begin,modelTime$end,by=modelTime$frequency)
#     assign("modelTime",modelTime,envir = .GlobalEnv)
#   }
# if(is.null(nsim)){assign("nsim",10000,envir = .GlobalEnv)}
  # setup up the time of the model run.
  # read in the model nodes and contruct the DAG
#   graph.nodes = pg.spi.exec(paste("select * from ",schema,".",nodeTable,sep=""))
#   graph.edges = pg.spi.exec(paste("select * from ",schema,".",nodeTable,"_edges",sep=""))
  #g = ftM2graphNEL(cbind(graph.edges$"from_name",graph.edges$"to_name"))
  g = ftM2graphNEL(cbind(graph.edges$node_from,graph.edges$node_to))
  # Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g)); # Rgraphviz::renderGraph(plot.graph("watershed","model"))
  # Node names may have spaces. Replace them with "_" via 'gsub.'
#   nodes(g) = gsub(" ","_",nodes(g))
#   graph.nodes$name = gsub(" ","_",graph.nodes$name)
  # keep only nodes that have edges (ie, drop orphan nodes)
  #used_nodes <- graph::nodes(g)
  # ind = match(nodes(g),graph.nodes$name)
#   graph.nodes = graph.nodes[graph.nodes$name %in% nodes(g),]
#   row.names(graph.nodes) = graph.nodes$name
#   complete = simulcalcs(g=g,graph.nodes=graph.nodes,schema,nsim=nsim)
  return("{'message':'complete'}")
}

simulcalcs <- function(g,graph.nodes,scenarios.nodes,schema,nsim=5){
  # simulcalcs.R
  # Will Barnett and Tom Stockton, November 2011.
  # simulcalcs() will perform Monte Carlo simulations of DAG. It calls
  # simulnode() for each node in the graph, and returns
  # a storage data frame with the node ids, node names, and simulated values.
  # g = DAG in the graphNEL object form.
  # graph.nodes = the data frame computed from the SQL query.
  # nsim = number of simulations desired.
  require(rjson)
  probs = seq(0.01,0.99,by=0.01)
  # Figure out the longest ancestry in the graph.
  # anc = apply(gRbase::as.adjMAT(g),2,sum)
  # Find out which nodes are roots; i.e., no parents. And how many of them there are.
  # roots = nodes(g)[apply(gRbase::as.adjMAT(g),2,sum) == 0]
  # leaves = nodes(g)[apply(gRbase::as.adjMAT(g),1,sum) == 0]
  roots = graph::leaves(g,"in")
  leaves = graph::leaves(g,"out")
  # Simulate nodes sequentially and store results.
  optionNodes = graph.nodes$name
  [graph.nodes$type == "option"]
  stochasticNodes = graph.nodes$name[grepl("distribution",graph.nodes$specification) &
                                       !grepl("equation|function|time",graph.nodes$specification)]
  #scenarioNodes = scenarios #pg.spi.exec(paste("select * from ",schema,".","management_scenarios",sep=""))
  if( is.null(scenarioNodes) || nrow(scenarioNodes) == 0 ) scenarioNodes = data.frame(id=1,name="Base Case")
  # Initialize list to hold results
  level.list = vector("list",nrow(graph.nodes))
  names(level.list) = nodes(g)
  scenLevel.list = vector("list",length(scenarioNodes$id))
  names(scenLevel.list) = scenarioNodes$name
  value.list = vector("list",nrow(graph.nodes[graph.nodes$type %in% c("measure"),]))
  names(value.list) = graph.nodes$name[graph.nodes$type %in% c("measure")]
  index.list = vector("list",length(scenarioNodes$id))
  names(index.list) = scenarioNodes$name
  scenValue.list = vector("list",length(scenarioNodes$id))
  names(scenValue.list) = scenarioNodes$name
  scenIndex.list = vector("list",length(scenarioNodes$id))
  names(scenIndex.list) = scenarioNodes$name

  if(length(stochasticNodes)==1){nsims=1}
  # Start while loop to iterate through the 'levels' of the network. Each level is a subset of nodes which have
  # previously-simulated parents (or none at all, in the case of the root nodes).
  level = roots
  nodesNotDone = nodes(g) # The while loop requires nodesNotDone to be > 0.
  nodesDone = NULL
  while( length(nodesNotDone) > 0 ){
    # loop through current set of unsolved root nodes
    for( thisLevel in level ){ #   thisLevel = level[1]
      cat(thisLevel,"\n")
      nodesDone = c(nodesDone,thisLevel)
      nodesNotDone = nodesNotDone[!grepl(thisLevel,nodesNotDone)]
      nodeSpec = fromJSON(
        graph.nodes$specification[graph.nodes$id==thisLevel][[1]][[1]]$rows
        )
      nodeSpec$id = graph.nodes[thisLevel,"id"]
      nodeSpec$class = ifelse(!is.null(graph.nodes[thisLevel,"class"]),graph.nodes[thisLevel,"class"],graph.nodes[thisLevel,"type"])
      thisNodeTable = switch(nodeSpec$class, "index" = "indices", "measure" = "measurable_attributes", "state" = "states")
      if( is.null(nodeSpec$type) ) { nodeSpec$type = "missing" }
      for(thisScen in scenarioNodes$name){  # thisScen = scenarioNodes$name[1]
        scenId = scenarioNodes$id[scenarioNodes$name == thisScen]
        nodeLevels = simulnode(spec=nodeSpec,data=scenLevel.list[[thisScen]],n=nsim,scenId=scenId)
        scenLevel.list[[thisScen]][[thisLevel]] = nodeLevels
        # calculate the value function for measures
        if( nodeSpec$class == "measure" ){
          thisNodeValueFunction = as.data.frame(do.call("rbind",nodeSpec$rows))
          value.list[[thisLevel]] = signif(try(approx(x=thisNodeValueFunction$level,y=thisNodeValueFunction$value,xout=nodeLevels,rule=2)$y),6)
          scenValue.list[[thisScen]] = value.list
          nodeValues.df = as.data.frame(cbind(prob=probs,do.call("cbind",lapply(scenValue.list,function(scen,thisLevel,probs){quantile(scen[[thisLevel]],probs)},thisLevel,probs))))
          nodeValuesJson = paste(apply(nodeValues.df,1,toJSON),collapse=",")
          nodeValuesJson = paste('{"metaData":{"root":"values","fields":',toJSON(names(nodeValues.df)),'},"values":[',nodeValuesJson,']}')
          ##          pg.spi.exec(paste("UPDATE ",schema,".",thisNodeTable," SET modeled_values = '",nodeValuesJson,"' WHERE id = ",nodeSpec$id,sep=""))
        } # calculate the value function [end]
      } # thisScen [end]

      if( nodeSpec$class == "index" ){
        # values
        for(thisScen in scenarioNodes$name){
          thisIndexList = list(with(scenValue.list[[thisScen]],eval(parse(text=nodeSpec$equation))))
          names(thisIndexList) = thisLevel
          index.list[[thisScen]] = thisIndexList
          scenLevel.list[[thisScen]][[thisLevel]] = thisIndexList[[1]]
        }
        nodeValues.df = as.data.frame(cbind(prob=probs,do.call("cbind",lapply(index.list,function(scen,thisLevel,probs){quantile(scen[[thisLevel]],probs)},thisLevel,probs))))
        nodeValuesJson = paste(apply(nodeValues.df,1,toJSON),collapse=",")
        nodeValuesJson = paste('{"metaData":{"root":"values","fields":',toJSON(names(nodeValues.df)),'},"values":[',nodeValuesJson,']}')
        ##        pg.spi.exec(paste("UPDATE ",schema,".",thisNodeTable," SET modeled_values = '",nodeValuesJson,"' WHERE id = ",nodeSpec$id,sep=""))
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
        ##        pg.spi.exec(paste("UPDATE ",schema,".",thisNodeTable," SET modeled_levels = '",nodeLevelsJson,"' WHERE id = ",nodeSpec$id,sep=""))
      }
    } # thisLevel [end]

    # Figure out which nodes are "next" in line. Find all the unique children of the current level,
    # and see which have parents that have data generated.
    levelKids = unique(gRbase::children(level,g))
    level = levelKids[unlist(lapply(levelKids,function(node,nodesDone){ all((gRbase::parents(node,g)) %in% nodesDone )  }, nodesDone ) )]
  } # while [end]

  # Build scenario comparison charts
  if( nrow(scenarioNodes) > 1 ) { #"index" %in% graph.nodes$type){
    index.df = as.data.frame(do.call("rbind",lapply(index.list,function(x){as.data.frame(do.call("cbind",lapply(x,mean)))})))
    measures.df = do.call("rbind",lapply(scenValue.list,function(x){as.data.frame(do.call("cbind",lapply(x,mean)))}))
    measures.df = cbind(name=row.names(measures.df),measures.df)
    measures.df = merge(measures.df,index.df,by="row.names",sort=FALSE)[,-1]
    comparisonHtml(data=measures.df,schema)
  }

  # Run the sensitivity analysis only if there are stochastic nodes
  if(length(stochasticNodes)>0){
    sims.df = as.data.frame(do.call("rbind",lapply(1:length(scenLevel.list),function(i){
      df = as.data.frame(do.call("cbind",scenLevel.list[[i]]))
      df$Scenario = names(scenLevel.list)[[i]]
      df
    })))
    savoi(sims.df[,leaves],sims.df[,c(optionNodes,stochasticNodes)],schema,graph.nodes)

    #     independent.df = as.data.frame(do.call("rbind",lapply(1:length(scenLevel.list),function(i){
    #       df = as.data.frame(do.call("cbind",scenLevel.list[[i]][stochasticNodes]))
    #       df = as.data.frame(do.call("cbind",scenLevel.list[[i]][stochasticNodes]))
    #       df$Scenario = names(scenLevel.list)[[i]]
    #       df
    #     })))
    #     if( "option" %in% graph.nodes$type ){
    #       for(i in 1:length(graph.nodes$name[graph.nodes$class == "option"])){ # i = 1
    #         thisName = graph.nodes$name[graph.nodes$class == "option"][i]
    #         thisSet = do.call("rbind",lapply(fromJSON(graph.nodes$specification[graph.nodes$name == thisName])$row,function(x)data.frame(level=x$level,Scenario=x$name)))
    #         if(i ==1 ) {
    #           optionsLevels = thisSet
    #         }else{
    #           optionsLevels = merge(optionsLevels,thisSet,by="Scenario")
    #         }
    #       }
    #       names(optionsLevels) = c("Scenario",graph.nodes$name[graph.nodes$class == "option"])
    #       #independent.df = merge(independent.df,optionsLevels,sort=F)[,-1]
    #       independent.df = merge(independent.df,optionsLevels,sort=F)
    #     }
    #     independent.df$Scenario = as.factor(independent.df$Scenario)
    #     response.df = scenLevel.list[[1]][leaves][[1]]
    #     savoi(response.df,independent.df,schema,graph.nodes)
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
    #"Ext.create('Ext.tip.ToolTip',{target:'weights1',autoHide:false,closable:true,height:400,width:400,layout:'fit',items:",
    #"new Gisdt.dasees.ObjectivesInterface({id:'objectivesAttrCardTip',project:'",schema,"',category:'attributesGrid'})",
    #"new Gisdt.JitPanel({project:'",schema,"',nodesTable:'dasees',edgesTable:'obj_hier_edges',nodesFields:['id','name','dasees'],graphType:'JitSpacetree'})",
    #"new Gisdt.tree.DbTableTree({title:'',height:400,width:400,iconCls:'silk-wrench',project:'watershed',table:'objectives',edges_table:'edges',baseAttrs:{checked:null}})",
    #"});",
    #"var foo = new Gisdt.tree.DbTableTree({title:'',renderTo:'objs',height:400,width:400,iconClsLeaf:'icon-scales',collapsible:true,project:'watershed',table:'attr_obj',edges_table:'edges',checkbox:false,baseAttrs:{checked:null}});",
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
    sim <- with(data,eval(parse(text=spec$equation)))
  }else if(spec$type == "option"){
    tempsim = as.data.frame(do.call("rbind",spec$row))
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
    sim = with(data,eval(parse(text=spec$equation)))
  } else if ( spec$type=="data" ){
    sim = 0
  }
  return(sim)
}

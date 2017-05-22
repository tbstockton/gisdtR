tnormcpt = function(mu,variance,ntarget,nvpar){
  #' Solve Truncated Normals for Ranked Nodes
  #' @param mu function for mu of the truncated normal distribution.
  #' @param ntarget number of levels of the target node.
  #' @param nVpar vector of numbers of levels of each parent.
  #' @return conditional probability table.
  #' @export
  #' @examples
  #' mu='Staff+Testing'; nVpar=rjson::fromJSON('{"Staff": 5, "Testing": 5}'); ntarget = 5
  #' mu='20*Patrons+10*Reservation'; nVpar=rjson::fromJSON('{"Patrons": 3, "Reservation": 2}'); ntarget = 4
  #' mu='10'; nVpar=0; ntarget = 6; var=1
  #' mu='0.9'; var=0.1; nVpar=1; ntarget = 10;
  #' gisdtR::tnormcpt(mu=.01,var=.1,ntarget=5,nVpar=0)
  #' gisdtR::tnormcpt(mu=mu,var=var,ntarget=ntarget,nVpar=nVpar)
  #' gisdtR::
  #' tnormcpt(mu='0.1*Node53+0.1*Node71',variance='0.1',ntarget=2,nvpar='{"Node53": 3, "Node71": 2}')
  #' SELECT shared.gisdt_tnormcpt('0.1*Node53+0.1*Node71','0.1',2,'{"Node53": 3, "Node71": 2}')
  #' tnormcpt('2.3*Node72+3.7*Node59+5.3*Node74','0.65', 2,'{"Node72": 2, "Node59": 3, "Node74": 2}')
  expand.grid.tnormcpt = function(n)
  {
    args = lapply(n,function(n)(2*(1:n)-1)/(2*n))
    nargs <- length(n) #length(args <- list(...))
    cargs <- vector("list", nargs)
    names(cargs) = names(n)
    iArgs <- seq_len(nargs)
    rep.fac <- 1L
    d <- unlist(lapply(args,length)) #lengths(args) lengths not available in 3.1.2
    orep <- prod(d)
    for (i in iArgs) { # i = 1
      x <- args[[i]]
      nx <- length(x)
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
      cargs[[i]] <- x
      rep.fac <- rep.fac * nx
    }
    rn <- .set_row_names(as.integer(prod(d)))
    structure(cargs, class = "data.frame", row.names = rn)
  }

  nvpar = jsonlite::fromJSON(nvpar)
  ntarget = as.integer(ntarget)
  variance = as.numeric(variance)
  namesvpar = names(nvpar)
  names(namesvpar) = names(nvpar)
  tmeans = expand.grid.tnormcpt(n=nvpar)
  mu.normalizer = max(with(expand.grid(lapply(namesvpar,function(x)c(0,1))),eval(parse(text=mu))))
  mu.normalizer = ifelse(length(nvpar)>1,mu.normalizer,1)
  tsd = ifelse(is.null(variance),sqrt(1/mu.normalizer),sqrt(variance))
  tmeans = with(tmeans,eval(parse(text=mu)))/mu.normalizer
  cpt = sapply(tmeans,function(tmean){
    normalizer = diff(pnorm(c(0,1),tmean,tsd))
    (pnorm((1:ntarget)/ntarget,tmean,tsd)-pnorm((0:(ntarget-1))/ntarget,tmean,tsd))/normalizer
  })
  return(jsonlite::toJSON(round(as.vector(cpt),4)))
}


# Functions for individual ranked logit
# From https://docs.displayr.com/wiki/MaxDiff_Analysis_Case_Study_Using_R


library(BiasedUrn)
setup.flat.data = function(x, number.alternatives){
  n = nrow(x)
  number.sets = ncol(x) / number.alternatives
  data = vector("list",n)
  for (i in 1:n)
  {
    temp.respondent.data = matrix(x[i,],byrow=TRUE,ncol = number.alternatives)
    respondent.data = vector("list",number.sets)
    for (s in 1:number.sets)
      respondent.data[[s]] = as.numeric(temp.respondent.data[s,])
    data[[i]] = respondent.data
  }
  compress.data(data)}

compress.data = function(x){#Creates a vector for each set where the first entry was best and the last worst
  compress = function(x){
    x.valid = !is.na(x)
    x.position = (1:length(x))[x.valid]
    x.position[order(x[x.valid], decreasing = TRUE)]
  }
  n = length(x)
  number.sets = length(x[[1]])
  number.alternatives = length(x[[1]][[1]])
  data = vector("list",n)
  for (i in 1:n)
  {
    respondent.data = vector("list",number.sets)
    for (s in 1:number.sets)
      respondent.data[[s]] = compress(x[[i]][[s]])
    data[[i]] = respondent.data
  }
  class(data) = "maxdiffData"
  data}

d.marley = function(b,x){
  b.vector = b[x]
  k = length(b.vector)
  ediffs = exp(matrix(b.vector,k,k,byrow=FALSE) - matrix(b.vector,k,k,byrow=TRUE))
  ediffs[1,k] / (sum(ediffs) - sum(diag(ediffs)))}

d.rlogit = function(b,x){
  eb = exp(b[x])
  k = length(eb)
  d.best = eb[1]/sum(eb)
  d.not.worst = dMWNCHypergeo(c(rep(1,k-2),0), rep(1,k-1),k-2,eb[-1], precision = 1E-7)
  d.best * d.not.worst}

d.repeated.maxdiff = function(b,x, method){
  prod(as.numeric(lapply(x,b = b,switch(method,marley=d.marley, rlogit = d.rlogit))))}

ll.max.diff = function(b,x, maxdiff.method = c("marley","rlogit")[1]){
  b[b > 100] = 100
  b[b < -100] = -100
  sum(log(as.numeric(lapply(x,b = c(0,b),method=maxdiff.method, d.repeated.maxdiff))))
}

max.diff.rank.ordered.logit.with.ties = function(stacked.data){
  flat.data = setup.flat.data(stacked.data, ncol(stacked.data))
  solution = optim(seq(.01,.02, length.out = ncol(stacked.data)-1), ll.max.diff,  maxdiff.method  = "rlogit", gr = NULL, x = flat.data, method =  "BFGS", control = list(fnscale  = -1, maxit = 1000, trace = FALSE), hessian = FALSE)
  pars = c(0, solution$par)
  names(pars) = dimnames(stacked.data)[[2]]
  list(log.likelihood = solution$value, coef = pars)}

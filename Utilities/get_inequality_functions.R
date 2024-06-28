
#Functions for Gini, Theil and MLD using custom scaling parameter
#rather than the mean. 

# Permanyer-Seth-Yalonetzky (PSY) absolute Gini index
# Absolute Gini normalised by mean(max_value - mean)
PSY_ineq = function(vec, max_v = 1){
  
  vec = vec[!is.na(vec)]
  mean_vec = mean(vec)
  n =  length(vec)
  
  sum_diff = 0
  for (i in 1:n) {
    for (j in 1:n) {
      sum_diff = sum_diff + abs(vec[i] - vec[j])
    }
  }
  
  GINI_abs = sum_diff/(2*(n^2))
  PSY = GINI_abs / (mean_vec*(max_v - mean_vec))
    
  return(PSY)
}

#Erreygers corrected concentration index
erreygers_ineq = function(vec, min_v = 0, max_v = 1){
  vec = vec[!is.na(vec)]
  n = length(vec)
  rank_vec = rank(vec, ties.method = "average") / n
  sum_vec = 0
  for (i in 1:n){
    b = (vec[i] - min_v) / (max_v - min_v)
    sum_vec = sum_vec + (4 * b * (2 * rank_vec[i] - 1))
  }
  
  erreygers = sum_vec / n
  return(erreygers)
}

# Absolute Gini coefficient
GSC_ineq = function(vec, min_v = 0, max_v = 1, aversion = 3){
  
  vec = vec[!is.na(vec)]
  mean_vec = mean(vec)
  n =  length(vec)
  
  sum_diff = 0
  for (i in 1:n) {
    for (j in 1:n) {
      sum_diff = sum_diff + abs(vec[i] - vec[j])
    }
  }
  
  GINI_abs = sum_diff/(2*(n^2))
  return(GINI_abs)
}

# End of script


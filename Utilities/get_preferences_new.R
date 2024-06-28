
#This function returns preference data based on ranking questions over 8 buyer preference domains
# The mlogit function performs a mixed logit model/hierarchial bayes model for preferences.
# Individuals ranked attibutes from 1-4 over 6 questions. 
# We convert this data to consider pairwise comparisons within a best-worst framework. 
# This allows for binary decisions between two choices in the mlogit model. 

get_preferences = function(data) {

  #Experimental design
  questions = data.frame("question_1" = c('open','exten','credit','listens'),
                         "question_2" = c('momopay','price','credit','ease'),
                         "question_3" = c('open','price','stable','credit'),
                         "question_4" = c('momopay','exten','stable','listens'),
                         "question_5" = c('open','momopay','ease','stable'),
                         "question_6" = c('price','exten','ease','listens'))
  
  #Create references for us on what each digit means. 
  attribute_vec = data.frame("open" = 1,
                             "exten" = 2,
                             "credit" = 3,
                             "listens" = 4,
                             "momopay" = 5,
                             "price" = 6,
                             "ease" = 7,
                             "stable" = 8)
  
  #Set up empty dataframes to fill and rbind. 
  data_long = as.data.frame(matrix(NA, ncol = 8, nrow = 1))
  colnames(data_long) = c("open", "exten", "credit", "listens", "momopay", "price", "ease", "stable")
  data_long_all = data_long[-1,]
  
  col_left = which(names(data) == "BUYER_PREF_1_1")
  col_right = which(names(data) == "BUYER_PREF_6_4")
  
  for (hh in 1:nrow(data)){
    
    #Numeric vector of each individual of rankings
    rankings = as.numeric(data[hh,c(col_left:col_right)])
    rankings = matrix(rankings, nrow = 6, byrow = TRUE)
    rankings =  as.data.frame(rankings)
    
    for (q in 1:6) {
      ranks = rankings[q,] # Pairwise set
      question_vec = questions[,q]
      
      data_long[1,question_vec[1]] = ranks[1]
      data_long[1,question_vec[2]] = ranks[2]
      data_long[1,question_vec[3]] = ranks[3]
      data_long[1,question_vec[4]] = ranks[4]
      
      
      #append to master dataframe
      data_long_all= rbind(data_long_all, data_long)        
      
      #Empty dataframe for next loop
      data_long[] = NA
    }
  }
  
  nAlternatives = ncol(attribute_vec) # possible alternatives
  nBlocks = 6 # Number of pairwise comparisons made per obs
  nAltsPerSet = 4 #Alternatives shown per pairwise comparison
  n = nrow(data) #observations
  nObservations = n * nBlocks  #total pairwise comparisons. 
  
  #Get summary ranking of preferences from counts. 
  counts = apply(data_long_all, 2, mean, na.rm = TRUE)
  ranks = nAlternatives + 1 - rank(counts)
  cbind(Counts = counts, Ranks = ranks)
  
  # Derive individual counts. 
  id = rep(1:n,rep(nBlocks,n))
  individualCounts = aggregate(data_long_all, list(id),mean, na.rm = TRUE)[,-1]
  
  #Get individual rankings
  ranks = nAlternatives + 1 - apply(individualCounts,1,rank) #ranks
  rankProportions_tables = apply(ranks,1,table)
  
  rankProportions = matrix(0, nrow = nAlternatives, ncol = (nAlternatives*2)-1)
  colnames(rankProportions) =  seq(1, 8, by = 0.5)
  
  for (r in 1:nAlternatives){
    row = t(rankProportions_tables[[r]])
    for (col in 1:ncol(rankProportions)){
      colname = colnames(rankProportions)[col]
      if (colname %in% colnames(row)){
        rankProportions[r,col] =  row[1, colname] 
      } else {
        rankProportions[r,col] = 0
      }
    }
  }
  
  round(rankProportions,1)
  
  #And cumulative proportions
  rankCumProportions = t(apply(rankProportions,1,cumsum))
  round(rankCumProportions,1)
  
  # Individual counts
  id = rep(1:n,rep(nBlocks,n))
  individualCounts = aggregate(data_long_all,list(id),mean, na.rm = TRUE)[,-1]
  
  source(here("Utilities", "get_ranked_logit.R"))
  
  individualRankLogit = individualCounts
  stackedID = rep(1:n,rep(nBlocks,n))
  getRankCoefficients = function(id){max.diff.rank.ordered.logit.with.ties(data_long_all[stackedID == id,])$coef}
  for (i in 1:n){
    individualRankLogit[i,] = getRankCoefficients(i)
  }
  set.seed(0) #setting the random number seed so that random numbers are consistently applied across the examples
  ranks = nAlternatives + 1 - t(apply(individualRankLogit+ matrix(runif(n * nAlternatives)/100000, n),1,rank)) #ranks
  rankProportions = t(apply(t(ranks),1,table) / n * 100)
  round(rankProportions,3)
  
  shares = prop.table(as.matrix(exp(individualRankLogit)),1)
  shares = round(shares,3)
  
  data$INPUT_PREFERENCES =  data$EASE_PREFERENCES =  data$PRICE_PREFERENCES = rep(NA, nrow(data))
 
  for (r in 1:nrow(data)){
    data$INPUT_PREFERENCES[r] = sum(shares[r,2], shares[r,3])
    data$EASE_PREFERENCES[r] = sum(shares[r,1], shares[r,7])
    data$PRICE_PREFERENCES[r] = shares[r,6]
  }
  
  return(list(data, round(rankProportions,3), individualRankLogit))
}




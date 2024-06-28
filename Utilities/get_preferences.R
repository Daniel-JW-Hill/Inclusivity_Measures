
#This function returns preference data based on ranking questions over 8 buyer preference domains
# The Plackett-Luce model from Plackett (1975) and Luce (1959) to retrieve the individual preference weights 
# based on the ranked data in the survey. THis is achieved through the PlackettLuce package.

# See the following reference:
# Turner, H.L., van Etten, J., Firth, D. and Kosmidis, I., 2020. 
# Modelling rankings in R: the PlackettLuce package. Computational Statistics, 35(3), pp.1027-1057.

get_preferences = function(data) {
  
  data$INPUT_PROVISION_PREFERENCES = rep(0,nrow(data))
  data$EASE_MARKETING_PREFERENCES = rep(0,nrow(data))
 
  library(PlackettLuce)
  
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
  
  combinations = combn(1:8, 2)
  first_digits = combinations[1, ]
  second_digits = combinations[2, ]
    
  for (hh in 1:nrow(data)){
    
    #Create matrix of pairwise comparisons
    pairwise = data.frame("i" = first_digits,
                          "j" = second_digits,
                          "w_ij" = rep(0, length(first_digits)),
                          "w_ji" = rep(0, length(first_digits)))
                          # "t_ij" = rep(0, length(first_digits)))
    
    #Change numbers to attributes in pairwise frame
    pairwise[pairwise == 1] = "open"
    pairwise[pairwise == 2] = "exten"
    pairwise[pairwise == 3] = "credit"
    pairwise[pairwise == 4] = "listens"
    pairwise[pairwise == 5] = "momopay"
    pairwise[pairwise == 6] = "price"
    pairwise[pairwise == 7] = "ease"
    pairwise[pairwise == 8] = "stable"
  
    #Populate pairwise matrix with wins and losses for each preference (no ties)
  
    #Numeric vector of each individual
    rankings = as.numeric(data[hh,c(122:145)])
    rankings = matrix(rankings, nrow = 6, byrow = TRUE)
    rankings =  as.data.frame(rankings)
    
    for (q in 1:6) {
      seen_combinations <- matrix(FALSE, nrow = 4, ncol = 4)
      ranks = rankings[q,] # Pairwise set
      for (i in 1:4) {
        for (j in 1:4) {
          if (i != j && !seen_combinations[i, j]) { # Check if combination is not seen
            ii = questions[i,q] # Pairwise option 1
            jj = questions[j,q] # Pairwise option 2
            row_idx = which(pairwise$i %in% c(ii, jj) & pairwise$j %in% c(ii, jj)) # Find pairwise row in pairwise dataframe
            pairwise[row_idx, 3] = ifelse(ranks[i] > ranks[j], pairwise[row_idx, 3] + 1, pairwise[row_idx, 3])
            pairwise[row_idx, 4] = ifelse(ranks[i] > ranks[j], pairwise[row_idx, 4], pairwise[row_idx, 4] + 1)
            seen_combinations[i, j] = seen_combinations[j, i] =  TRUE # Mark combination as seen so we do not duplicate pairwise combination in the table. 
          }
        }
      }
    }
  
    #Set up for PlacketLuce package
    i_wins = data.frame(Winner = pairwise$i, Loser = pairwise$j)
    j_wins = data.frame(Winner = pairwise$j, Loser = pairwise$i)
    
    pairwise_new = as.rankings(rbind(i_wins, j_wins),
                      input = "orderings")
    
    weights = unlist(pairwise[c("w_ij", "w_ji")])
    
    #Override issue of rare zero obs
    weights = weights*2
    weights[weights == 0]= 1
    
    #Estimate the model
    model =  PlackettLuce(pairwise_new, weights = weights, npseudo = 0, maxit = 10)
    coefs = coef(model, ref = NULL, log = TRUE)
    coefs = as.data.frame(coefs)
    
    data$INPUT_PROVISION_PREFERENCES[hh] = mean(coefs["credit",],
                                            coefs["exten",])
    
    data$EASE_MARKETING_PREFERENCES[hh] = mean(coefs["ease",],
                                           coefs["listens",],
                                           coefs["momopay",],
                                           coefs["open",])
    
    data$PRICE_PREFERENCES[hh] = mean(coefs["price",],
                                       coefs["stable",])
    }

  return(data)
}

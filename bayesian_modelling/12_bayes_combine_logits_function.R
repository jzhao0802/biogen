# The following function combines logits via a weighted sum:
# The idea is to combine logits into a sensible weighted sum based
# on input from both the experts at Biogen and our predictive model.
combine_logits <- function(input_data, weights, min_model_weight,
                           model_logit_column = "xgb_logit",
                           patient_ID_col = "pat_id") {
  
  # find column for each weighted feature in input data
  weights$col_index <- grep(paste(weights$feature, collapse = "|"), colnames(input_data))
  
  # find the column with the model logit scores
  model_index <- grep(model_logit_column, colnames(input_data))
  
  # scale the weights so they sum to (1 - min_model_weight):
  weights$scaled <- (weights$weight/sum(weights$weight)) - 
    (min_model_weight)/ length(weights$weight)
  
  # multiply each Bayesian column by its respective weight:
  for(i in weights$col_index) {
    input_data[,i] <- weights$scaled[weights$col_index == i] * input_data[,i]
  }
  
  # for each row in input data, find the number of non-zero bayes features,
  # rescale the model weight so that the non-zero weights add up to 1, 
  # then sum up all the weights to give an weighted model score.
  logits_weighted <- apply(input_data, 1, function(row) { 
    
    row <- as.numeric(as.character(row))
    
    # which parts of the row are non-zero
    non_zero_weights <- which(row[weights$col_index] != 0)
    
    # re-calculate the model weight based on the non-zero bayesian variables:
    min_model_weight <- 1 - sum(weights$scaled[non_zero_weights])
    
    # multiply the model score by the adjusted min_model_weight:
    row[model_index] <- row[model_index] * min_model_weight
    
    # sum over the weighted scores to give a single score
    logit_weighted <- sum(row[model_index], row[weights$col_index])
    
    return(logit_weighted)
    
  })
  
  return(data.frame(pat_id = input_data[patient_ID_col], sum_weighted_logit = logits_weighted))
  
}

# ~ -----------------------------------------------------------------------
# FUNCTIONS FOR BIOGEN MODELLING PIPELINE
# ~ -----------------------------------------------------------------------

# FUNCTION TO CREATE EVENLY SPACED SEQUENCE OVER A FEATURE SPACE --------
get_feature_space <- function(feature) {
  min_feature <- min(feature, na.rm = TRUE)
  max_feature <- max(feature, na.rm = TRUE)
  feature_seq = seq(min_feature, max_feature,length=100)
  return(feature_seq)
}


# FUNCTION TO ESTIMATE DENSITY FUNCTIONS ----------------------------------
# This fuction estimates a density function for a feature
# Used to estimate likelihood functions.
fit_kde <- function(x) {x
  kde = density(x, na.rm = TRUE)
  f=approxfun(kde$x, kde$y, yleft=0, yright=0)
  return(f)
}


# FUNCTION TO ESTIMATE LIKELIHOODS ----------------------------------------
# this function estimates likelihood functions for each class. If there are
# fewer than 2 obs for a class, the likelihood etimated from the wider
# dataset is used instead:
train_bayes <- function(feature, label, model__prior) {
  
  N_0 = sum(!is.na(feature[label==0]))
  N_1 = sum(!is.na(feature[label==1]))
  x_space = get_feature_space(feature)
  
  if (N_1 <=2) {
    # use likelihood from step 1 if too sparse
    llh_1 = model_prior$llh1
  } else {
    # compute likelihood function:
    llh_1 = fit_kde(feature [label==1])
  }
  
  if (N_0 <=2) { 
    # use likelihood from step 1 if too sparse
    llh_0 = model_prior$llh0
  } else {
    # compute likelihood function
    llh_0 = fit_kde(feature [label==0] )
  }
  
  return(list(llh0 = llh_0, llh1 = llh_1, 
              N0 = N_0, N1 = N_1,
              x_space = x_space))
  
  
  # return(list(llh1 = llh_1, llh0 = llh_0))
  
}


# FUNCTION TO COMPUTE PDF OF FEATURE --------------------------------------
# This function returns the likelihood of outcome 1 divided by the sum of the
# likelihoods which is equivalent to the unconditional pdf of the feature.

predict_bayes <- function(model, feature) {
  scores = model$llh1(feature) /(model$llh1(feature) + model$llh0(feature))
  return(scores)
}


# FUNCTION TO COMPUTE LOG LIKELIHOODS -------------------------------------
# This outputs log likelihood ratios (logit) for each observation, replacing
# NA with 0.
predict_bayes_logit <- function(model, feature) {
  scores = log(model$llh1(feature)+.Machine$double.xmin) -
    log(model$llh0(feature)+.Machine$double.xmin)
  selected = is.na(scores)
  scores[selected] = 0
  return(scores)
}


# FUNCTION TO COMBINE LOGITS VIA A WEIGHTED SUM ---------------------------
# The following function combines logits via a weighted sum:
# The idea is to combine logits into a sensible weighted sum based
# on input from both the experts at Biogen and IQVIA's predictive model.
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


# FUNCTION TO TRANSFORM PROBABILITIES/LIKELIHOOD TO LOGITS --------------------
logit_transform <-function(x) {
  x[is.na(x)] = 0.5
  return(log(x) - log(1-x))
}




# FUNCTIONS ---------------------------------------------------------------

# function to create evenly spaced sequence from min to mx of a feature
get_feature_space <- function(feature) {
  min_feature <- min(feature, na.rm = TRUE)
  max_feature <- max(feature, na.rm = TRUE)
  feature_seq = seq(min_feature, max_feature,length=100)
  return(feature_seq)
}

# This fuction estimates a density function for a feature
# Used to estimate likelihood functions.
fit_kde <- function(x) {x
  kde = density(x, na.rm = TRUE)
  f=approxfun(kde$x, kde$y, yleft=0, yright=0)
  return(f)
}

# this function estimates likelihood functions for each class. If there are
# fewer than 2 obs for a class, the llh is given a gaussian form.
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

# This function returns the likelihood of outcome 1 divided by the sum of the
# likelihoods which is equivalent to the unconditional pdf of the feature.
# This is therefore a direct application of Bayes' Theorem to the data.
# The resulting scores are posterior probabilities.
predict_bayes <- function(model, feature) {
  scores = model$llh1(feature) /(model$llh1(feature) + model$llh0(feature))
  return(scores)
}

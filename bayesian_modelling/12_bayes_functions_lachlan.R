# FUNCTIONS ---------------------------------------------------------------


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


inference_bayes <- function(model_bayes, x, legend_label = 'Prob-Bayes') {
  
  if (missing(x)) {
    x = model_bayes$x_space
  }
  
  prob_ = predict_bayes(model_bayes, x)
  mat =  rbind( tibble(x=x, y = model_bayes$llh0(x) * 20, type='llh 0'),
                tibble(x=x, y = model_bayes$llh1(x) * 20, type='llh 1'),
                tibble(x=x, y = prob_ , type =legend_label)
  )
}


logit_transform <-function(x) {
  x[is.na(x)] = 0.5
  return(log(x) - log(1-x))
}

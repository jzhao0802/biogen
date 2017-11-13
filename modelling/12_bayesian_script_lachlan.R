
# Notes -------------------------------------------------------------------
# The output of the inference function has three 'types', llh 0,
# llh 1, and Prob-Bayes. Prob-Bayes = llh 1/(llh 1 + llh 0)
# ~ -----------------------------------------------------------------------



# ~ -----------------------------------------------------------------------
# BAYESIAN PIPELINE  ------------------------------------------------------
# ~ -----------------------------------------------------------------------

# Globals -----------------------------------------------------------------

library(tidyverse)
library(mlr)
library(stringr)

data_dir <- "F:/Projects/Biogen_Tecfidera/Data/Processed/"

weights_vector <- c(1:8)

# Load in the data --------------------------------------------------------

combined <- read_rds(paste0(data_dir, "combined_date_complied_rectified_num_gaba_copay_data.rds"))
config = read_csv(paste0(data_dir, "combined_date_complied_rectified_num_gaba_copay_config.csv"))

bayes_vars <- c('post_symps_fst8_diff','post_symps_fst10_diff','post_symps_fst12_diff',
                'post_symps_fst13_diff','post_symps_fst14_diff','post_symps_fst15_diff',
                'post_symps_fst16_diff', 'post_dme_fst_diff')

# STEP 1
# Estimate likelihood functions based on more densely populated features.
# This is to be used in the case where a feature is too sparsely populated
# to estimate a density function of its own:

# extract the variables in the model which are densely populated
model_varlist <- config$Column[config$v2 == TRUE | config$v3 == TRUE]
model_varlist <- model_varlist[grep("diff", model_varlist)]
model_varlist <- setdiff(model_varlist, bayes_vars)
combined_subset <- combined[model_varlist]

# stack these densely populated variables on top of each other
combined_stack <- data.frame(stack(combined_subset), label = rep(combined$discontinue_flg, ncol(combined_subset)))
# remove missing values
combined_stack <- combined_stack[!is.na(combined_stack$values), ]
# compute a likelihood function for these stacked variables
model_prior = train_bayes(combined_stack$values, combined_stack$label)

# test inference on a feature space.
# test_space = get_feature_space(combined_stack$values)  
# test_infer = inference_bayes(model_prior, x_)

# STEP 2
# estimate a likelihood function for each of these features ---------------

# extract features from dataset:
bayes_data <- combined[c(bayes_vars, "discontinue_flg")]

# compute likelihood functions for each of these features:
bayes_likelihood <- lapply(bayes_data[, 1:8], 
                     function(x) { train_bayes(feature = x, label = bayes_data_red$label, model__prior = model__prior)})

# feature space
bayes_feat_space <- as.data.frame(sapply(bayes_data[,1:8], get_feature_space))



sum(!is.na(bayes_data$post_symps_fst16_diff[bayes_data$discontinue_flg == 1]))



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
  
  # return(list(llh0 = llh_0, llh1 = llh_1, 
  #             N0 = N_0, N1 = N_1,
  #             x_space = x_space))
  # Changing this function's output to just the likelihoods:
  
  return(list(llh1 = llh_1, llh0 = llh_0))
  
}

# This function returns the likelihood of outcome 1 divided by the sum of the
# likelihoods which is equivalent to the unconditional pdf of the feature.
# This is therefore a direct application of Bayes' Theorem to the data.
# The resulting scores are posterior probabilities.
predict_bayes <- function(model, feature) {
  scores = model$llh1(feature) /(model$llh1(feature) + model$llh0(feature))
  return(scores)
}

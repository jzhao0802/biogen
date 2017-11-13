
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
source("F:/Lachlan/biogen_tecfidera/bayesian_modelling/12_bayes_functions_lachlan.R")

data_dir <- "F:/Projects/Biogen_Tecfidera/Data/Processed/"
results_dir <- "F:/Projects/Biogen_Tecfidera/Results/modelling_09_bayesian/LM_results/"

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

# extract some date difference "diff" variables from models 2 and 3 that are densely populated
model_varlist <- config$Column[config$v2 == TRUE | config$v3 == TRUE]
model_varlist <- model_varlist[grep("diff", model_varlist)]
model_varlist <- setdiff(model_varlist, bayes_vars)
combined_subset <- combined[model_varlist]

# stack these densely populated variables on top of each other
combined_stack <- data.frame(stack(combined_subset), 
                             label = rep(combined$discontinue_flg, 
                                         ncol(combined_subset)))
# remove missing values
combined_stack <- combined_stack[!is.na(combined_stack$values), ]
# compute a likelihood function for these stacked variables - this in intended
# to be extremely general:
model_prior = train_bayes(combined_stack$values, combined_stack$label, model__prior = NULL)
write_rds(model_prior, paste0(results_dir, "stacked_prior_likelihood_model.rds"))
model_prior <- read_rds(paste0(results_dir, "stacked_prior_likelihood_model.rds"))

# test inference on a feature space.
# test_space = get_feature_space(combined_stack$values)  
# test_infer = inference_bayes(model_prior, x_)

# STEP 2
# estimate a likelihood function for each of these features ---------------

# extract features from dataset:
bayes_data <- combined[c(bayes_vars, "discontinue_flg")]

# compute likelihood functions for each of these features, along with the feature
# space and the number of positives and negatives for that feature:
bayes_likelihood <- lapply(bayes_data[, 1:8], 
                     function(x) { train_bayes(feature = x, 
                                               label = bayes_data$discontinue_flg, 
                                               model__prior = model__prior)})

# # feature space
# bayes_feat_space <- as.data.frame(sapply(bayes_data[,1:8], get_feature_space))

# Run cross validation on the data to get a full set of predictions for
# each variable

bayes_cv_preds <- lapply(bayes_data[,1:8], function(feature, label = bayes_data$discontinue_flg,
                                                    model_prior = model_prior, k = 3) {
  
  # create dataframe of label and feature
  cv_data <- data.frame(label = label, feature = feature)
  
  # remove missing values
  cv_data <- cv_data[!is.na(cv_data$feature),]
  
  # assign each observation to one of k groups for cross validation
  cv_data$fold <- dismo::kfold(x = cv_data, k = k)
  
  # define an empty list for storing predictions
  pred_results <- list()
  
  # for each of these k groups, train a classifier on all other data, then
  # use it to predict for group m:
  
  for(m in 1:k) {
    
    # define the training set
    train_set <- cv_data[cv_data$fold != m, ]
    
    # define the prediction_set
    pred_set <- cv_data[cv_data$fold == m, ]
    
    # train classifier using training set
    train_cv <- train_bayes(feature = train_set$feature, label = train_set$label,
                            model__prior = model__prior)
    
    # predict on test set
    pred_cv <- predict_bayes_logit(model = train_cv, feature = pred_set$feature)
    
  }
  
})

cross_val_bayes <- function(data, label) {   }

sum(!is.na(bayes_data$post_symps_fst16_diff[bayes_data$discontinue_flg == 1]))




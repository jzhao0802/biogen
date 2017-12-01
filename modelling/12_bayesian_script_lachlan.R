
# ~ -----------------------------------------------------------------------
# BAYESIAN PIPELINE  ------------------------------------------------------
# ~ -----------------------------------------------------------------------

# Globals -----------------------------------------------------------------

library(tidyverse)
library(mlr)
library(stringr)
library(BBmisc)
source("F:/Lachlan/biogen_tecfidera/bayesian_modelling/12_bayes_functions_lachlan.R")
source("F:/Lachlan/biogen_tecfidera/bayesian_modelling/12_bayes_combine_logits_function.R")



# USER INPUTS -------------------------------------------------------------


# Provide the location of the data, the scores from XGBoost, and where the results
# are to be stored:
data_dir <- "F:/Projects/Biogen_Tecfidera/Data/Processed/"
weights_dir <- "F:/Projects/Biogen_Tecfidera/Data/Questionnaire_response/"
results_dir <- "F:/Projects/Biogen_Tecfidera/Results/modelling_09_bayesian/LM_results/"
xgboost_scores_dir <- "F:/Projects/Biogen_Tecfidera/Results/modelling_10_HP_optimisation/"

# Define the minimum model weight. This is the minimum amount of confidence
# the user has in the predictions from the model, relative to those from the
# bayesian analysis. This is subjective. In order to balance the influence
# of the model while preserving the input from the Bayesian analysis, we
# recommend a min_model_weight of 0.5:
min_model_weight <- 0.5

# Load in the data --------------------------------------------------------

combined <- read_rds(paste0(data_dir, "combined_date_complied_rectified_num_gaba_copay_data.rds"))
config = read_csv(paste0(data_dir, "combined_date_complied_rectified_num_gaba_copay_config.csv"))

# define some weights expressed as probabilities:
weights_df <- read_csv(paste0(weights_dir, "Biogen_responses_reformatted.csv"))
bayes_vars <- weights_df$Feature

# SANITY CHECK: Print the descriptions of these variables as a sanity check
bayes_descriptions <- config[config$Column %in% bayes_vars, c(1,4)]
View(bayes_descriptions)

# STEP 1 ------------------------------------------------------------------
# Estimate likelihood functions based on more densely populated features.
# This is to be used in the case where a bayes feature is too sparsely populated
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
# to be broad:
model_prior = train_bayes(combined_stack$values, combined_stack$label, model__prior = NULL)
# write it out:
write_rds(model_prior, paste0(results_dir, "stacked_prior_likelihood_model.rds"))
model_prior <- read_rds(paste0(results_dir, "stacked_prior_likelihood_model.rds"))


# STEP 2 ------------------------------------------------------------------
# estimate a likelihood function for each of these bayes features

# extract bayes features from dataset:
bayes_data <- combined[c(bayes_vars, "discontinue_flg", "pat_id")]

# compute likelihood functions for each of these features, along with the feature
# space and the number of positives and negatives for that feature.
# If a feature has too few observations to 
bayes_likelihood <- lapply(bayes_data[, 1:8], 
                     function(x) { train_bayes(feature = x, 
                                               label = bayes_data$discontinue_flg, 
                                               model__prior = model__prior)})

# # feature space
# bayes_feat_space <- as.data.frame(sapply(bayes_data[,1:8], get_feature_space))

# Run cross validation on the data to get a full set of predictions for
# each variable. This is an extensive operation, however the result is simply
# a set of predictions based on the baysian framework.
bayes_logit_preds <- lapply(bayes_data[,1:8], function(feature, label = bayes_data$discontinue_flg,
                                                    model_prior = model_prior, k = 3) {
  
  # create dataframe of label and feature
  cv_data <- data.frame(label = label, feature = feature)
  
  # assign each observation to one of k groups for cross validation
  cv_data$fold <- dismo::kfold(x = cv_data, k = k)
  
  # define a column of zeros to be populated with predictions
  cv_data$logit_scores <- NA
  
  # for each of these k groups, train a classifier on all other data, then
  # use it to predict for group m:
  
  for(m in 1:k) {
    
    # define the training set
    train_set <- cv_data[cv_data$fold != m, ]
    
    # remove missin values
    train_set <- train_set[!is.na(train_set$feature),]
    
    # define the prediction_set
    pred_set <- cv_data[cv_data$fold == m, ]
    
    # train classifier using training set
    train_cv <- train_bayes(feature = train_set$feature, label = train_set$label,
                            model__prior = model__prior)
    
    # predict on test set
    pred_cv <- predict_bayes_logit(model = train_cv, feature = pred_set$feature)
    
    # populate cv_data with predictions
    cv_data$logit_scores[cv_data$fold == m] <- pred_cv
    
  }
  
  # returns a list of logit scores from each outer fold of the cross validation
  return(cv_data$logit_scores)
  
})

# combine with patient id and label into one dataframe
bayes_logit_preds <- data.frame(label = bayes_data$discontinue_flg,
                             pat_id = bayes_data$pat_id,
                             bayes_logit_preds)
write_rds(bayes_logit_preds, paste0(results_dir, "bayes_logit_predictions.rds"))
bayes_logit_preds <- read_rds(paste0(results_dir, "bayes_logit_predictions.rds"))

# STEP 3 ------------------------------------------------------------------
# Load in the XGBoost predictions and combine them with the logits from the
# bayesian analysis

# Now that we have a list of logits from our Bayesian method, we need
# a list of results from the XGBoost cross validation so they can
# be combined:
xgb_resample <- read_rds(paste0(xgboost_scores_dir, "XGBoost_HP_optimised_resample_predictions.rds"))

# extract prediction object
predictions <- xgb_resample$pred

# convert XGBoost scores to logits
predictions$data$xgb_logit <- logit_transform(predictions$data$prob.1)

# join XGBoost's predictions onto the Bayesian predictions
pred_join <- inner_join(bayes_logit_preds, predictions$data[,8:9], by = "pat_id")

# ensure pred_join has correct column classes otherwise combining logits
# will not work:
pred_join[,3:ncol(pred_join)] <- sapply(pred_join[,3:ncol(pred_join)], 
                                        function(x) { as.numeric(as.character(x)) })

# combine the logit scores to produce one score:
logits_combined <- combine_logits(input_data = pred_join, 
                                  weights = weights_df, 
                                  min_model_weight = min_model_weight,
                                  model_logit_column = "xgb_logit")

# Note: please ignore the warnings here. They occur because we are coercing
# a character to a numeric (with no impact on the result).


# STEP 4 ------------------------------------------------------------------
# Evaluate performance

# create new prediction object using these logits (here we're repurposing the 
# prediction object from the model since performance measures tend to require
# a prediction object)
pred <- xgb_resample$pred

# remove redundant columns from the predictions data:
drop_cols <- c("id", "prob.0" , "response", "iter", "set")
pred$data[drop_cols] <- NULL

# add in the results of the logit:
pred$data$truth <- pred_join$label
pred$data$prob.1 <- logits_combined$sum_weighted_logit
pred$data$pat_id <- logits_combined$pat_id

# compute the area under the ROC curve:
aucROC <- mlr::performance(pred = pred, measures = auc)
print(aucROC)

# compare this to the area under the ROC curve for the original model:
auc_model <- mlr::performance(pred = xgb_resample$pred, measures = auc)
print(auc_model)

# STEP 5 ------------------------------------------------------------------
# Analysis of the patients for whom the Bayesian variables fire

# We can find the 'bayesian' patients (those for whom one at least
# one of the bayesian variables fires):
bayes_patients <- combined %>% filter_(paste("!is.na(", bayes_vars, ")", collapse = " |"))

# sort the model scores descending:

XGB_sort <- BBmisc::sortByCol(predictions$data, col = "xgb_logit", asc = FALSE)
combined_sort <- BBmisc::sortByCol(logits_combined, col = "sum_weighted_logit",
                                   asc = FALSE)

# investigate how many patients with these 'bayesian' variables are in the
# top 1000 patients from the model before and after the bayesian approach has
# been applied:

# Before (XGBoost model only):
length(which(XGB_sort$pat_id[1:1000] %in%  bayes_patients$pat_id))
# After (with Bayesian approach applied):
length(which(combined_sort$pat_id[1:1000] %in% bayes_patients$pat_id))

inverse_transform <- function(input) { exp(input) / (1 + exp(input))  }



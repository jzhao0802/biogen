
# ~ -----------------------------------------------------------------------
# Hyperparameter optimisation for Biogen model 3
# ~ -----------------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(mlr)
library(tidyverse)
library(xgboost)

# GLOBALS -----------------------------------------------------------------

data_dir <- "F:/Projects/Biogen_Tecfidera/Data/Processed/data_model_3_for_hyperparam_search_LM/"
results_dir <- "F:/Projects/Biogen_Tecfidera/Results/modelling_10_HP_optimisation/"

# DATA AND CONFIG FILE ----------------------------------------------------

var_config <- read_csv("F:/Projects/Biogen_Tecfidera/Data/Processed/combined_date_complied_rectified_num_gaba_copay_config.csv")
raw_data <- read_rds(paste0(data_dir, "data_model_3_for_HP_search.rds"))



# PREPROCESS --------------------------------------------------------------

# quick check looks like 714 out of 812 variables in data are in var_config
var_config$Description[grep(paste(var_config$Column, collapse = "|"), colnames(raw_data))]

# remove patient_id
patient_ids <- raw_data$pat_id
raw_data$pat_id <- NULL

# set label to factor
raw_data$label <- as.factor(raw_data$discontinue_flg_1_0)
raw_data$discontinue_flg_1_0 <- NULL

# correct for whitespace, slashes and hyphens in column names
colnames(raw_data) <- gsub(pattern = " ", replacement = "_", x = colnames(raw_data))
colnames(raw_data) <- gsub(pattern = "/", replacement = "_", x = colnames(raw_data))
colnames(raw_data) <- gsub(pattern = "-", replacement = "_", x = colnames(raw_data))

# MLR PIPELINE -----------------------------------------------------------

# create task
dataset <- makeClassifTask(id = "Data for HP search", data = data, 
                           target = "label", positive = 1)

# create learner
lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(nrounds = 100, 
                         verbose = FALSE, 
                         objective = "binary:logistic")

# make hyperparameter set:
ps <- makeParamSet(
  makeNumericParam("eta", lower=0.01, upper=0.3),
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeIntegerParam("min_child_weight", lower=1, upper=5),
  makeNumericParam("colsample_bytree", lower=.5, upper=1),
  makeNumericParam("subsample", lower=.5, upper=1)
)

# make resampling description and instance
rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")
rin <- makeResampleInstance(desc = rdesc, task = dataset)

# make random search iteration:
ctrl <- makeTuneControlRandom(maxit = 200)

# Define performane metrics
pr20 <- perf_make_pr_measure(recall_perc = 20, "pr20")
m2 <- mlr::auc

# tune parameters:
res_tune <- tuneParams(learner = lrn_xgb, task = dataset, resampling = rin,
                        par.set = ps, control = ctrl, 
                       measures = list(m2, pr20))

# save optimal parameters
write_rds(res_tune, paste0(results_dir, "resample_HP_random_search_object.rds"))
# res_tune <- read_rds(paste0(results_dir, "resample_HP_random_search_object.rds"))

# CV WITH OPTIMAL HYPERPARAMETERS ---------------------------------------

# create optimised learner
lrn_xgb_opt <- setHyperPars(lrn_xgb, par.vals = c(res_tune$x, nrounds = 100))

# run CV
res_opt <- resample(learner = lrn_xgb_opt, 
                    task = dataset, 
                    resampling = rin)

# add patient_id to resample predictions:
res_opt$pred$data$pat_id <- patient_ids[res_opt$pred$data$id]

# save optimised resample object
write_rds(res_opt, paste0(results_dir, "XGBoost_HP_optimised_resample_predictions.rds"))
# res_opt <- read_rds(paste0(results_dir, "XGBoost_HP_optimised_resample_predictions.rds"))


# EVALUATION --------------------------------------------------------------

# create PR curve
pr_opt <- perf_binned_perf_curve(pred = res_opt$pred, bin_num = 20)
# write out PR curve
write_csv(pr_opt$curve, paste0(results_dir, "XGBoost_HP_optimised_PR_curve.csv"))

# compute AUC of the model
mlr::performance(pred = res$pred, measures = auc)

# train a single model using these hyperparameters:
xgb_model <- train(learner = lrn_xgb_opt, task = dataset)

# write out model:
write_rds(xgb_model, paste0(results_dir, "Model_3_XGB_HP_optimised.rds"))


# VARIABLE IMPORTANCE -----------------------------------------------------

importance_model <- xgb.importance(feature_names = xgb_model$features,
                                   model = xgb_model$learner.model)
colnames(var_config)[1] <- "Feature"

# add the description column onto dataset:
importance_model_desc <- left_join(x = importance_model, y = var_config[,c(1:4)], by = "Feature")

# convert to numeric in order to use in detailed xgb.importance:
dataset_numeric <- as.data.frame(sapply(dataset$env$data, function(x) { as.numeric(as.character(x)) }))

# run detailed importance:
detailed_imp <- xgb.importance(feature_names = xgb_model$features,
                               model = xgb_model$learner.model, data = as.matrix(dataset_numeric),
                               label = dataset_numeric$label)
# add description to detailed importance:
detailed_imp_desc <- left_join(x = detailed_imp, y = var_config[, c(1:4)], by = "Feature")

# write out importances:
write_csv(importance_model_desc, paste0(results_dir, "Model_3_VI_single_model_HP_optimised.csv"))
write_csv(detailed_imp_desc, paste0(results_dir, "Model_3_Detailed_VI_single_model_HP_optimised.csv"))


# HCP TARGET LIST ---------------------------------------------------------

# read in raw dataset to extract NPI number
id_npi <- read_rds("F:/Projects/Biogen_Tecfidera/Data/Processed/combined_date_complied_rectified_num_gaba_copay_data.rds") %>% select(pat_id, npi, discontinue_flg, idx_dt)

# read in resample object to get predictions and join npi number
pred <- read_rds(paste0(results_dir, "XGBoost_HP_optimised_resample_predictions.rds"))$pred$data %>%
  left_join(y = id_npi, by = "pat_id") %>%
  select(pat_id, prob.1, truth, npi, idx_dt)

# read in HCP information
HCP <- read_csv("F:/Projects/Biogen_Tecfidera/Data/HCP_information/PHYSICIANS_NAME_ADDRESS.csv") %>%
  rename(npi = NPI)

# join HCP info to predictions
pred_hcp <- left_join(x = pred, y = HCP, by = "npi") %>%
  BBmisc::sortByCol(col = "prob.1", asc = FALSE) %>%
  rename(model_score = prob.1, discontinue_early = truth) %>% 
  mutate(idx_dt = mdy(idx_dt))

# write out joined dataset
write_csv(pred_hcp, paste0(results_dir, "HP_optimised_predictions_with_HCP_info.csv"))




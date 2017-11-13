
# ~ -----------------------------------------------------------------------
# Hyperparameter optimisation for Biogen model 3
# ~ -----------------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(mlr)
library(tidyverse)

# GLOBALS -----------------------------------------------------------------

data_dir <- "F:/Projects/Biogen_Tecfidera/Data/Processed/data_model_3_for_hyperparam_search_LM/"
results_dir <- "F:/Projects/Biogen_Tecfidera/Results/modelling_10_HP_optimisation/"

# DATA AND CONFIG FILE ----------------------------------------------------

var_config <- read_csv("F:/Projects/Biogen_Tecfidera/Data/Processed/combined_date_complied_rectified_num_gaba_copay_config.csv")
data <- read_rds(paste0(data_dir, "data_model_3_for_HP_search.rds"))


# PREPROCESS --------------------------------------------------------------

# quick check looks like 714 out of 812 variables in data are in var_config
var_config$Description[grep(paste(var_config$Column, collapse = "|"), colnames(data))]

# remove patient_id
patient_ids <- data$pat_id
data$pat_id <- NULL

# set label to factor
data$label <- as.factor(data$discontinue_flg_1_0)
data$discontinue_flg_1_0 <- NULL

# correct for whitespace, slashes and hyphens in column names
colnames(data) <- gsub(pattern = " ", replacement = "_", x = colnames(data))
colnames(data) <- gsub(pattern = "/", replacement = "_", x = colnames(data))
colnames(data) <- gsub(pattern = "-", replacement = "_", x = colnames(data))

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

# create PR curve
pr_opt <- perf_binned_perf_curve(pred = res_opt$pred, bin_num = 100)
# write out PR curve
write_csv(pr_opt$curve, paste0(results_dir, "XGBoost_HP_optimised_PR_curve.csv"))

# train a single model using these hyperparameters:






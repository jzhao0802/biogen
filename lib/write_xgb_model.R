write_xgb_model <- function(xgb_model, dataset, results_dir="./", description = NA) {
  # write out model:
  saveRDS(xgb_model, paste0(results_dir, "XGB_preliminary_model.rds"))
  
  # VARIABLE IMPORTANCE -----------------------------------------------------
  
  # variable importance for the single model ----------------------------------
  importance_model = xgb.importance(feature_names = xgb_model$features,
                                     model = xgb_model$learner.model)
  # convert to numeric in order to use in detailed xgb.importance:
  train_numeric = as.data.frame(
    sapply(dataset$env$data, function(x) { as.numeric(as.character(x)) }))
  
  detailed_imp = xgb.importance(
    feature_names = xgb_model$features,
    model = xgb_model$learner.model, 
    data = as.matrix(train_numeric),
    label = train_numeric$label)
  
  # write out:
  if ( is.na(description) ) {
    write_csv(importance_model, 
              paste0(results_dir, "VI_XGB_freq_singlemodel.csv"))
  } else {
    importance_model$Description = description [[importance_model$Feature]]
    write_csv(importance_model, 
              paste0(results_dir, "VI_XGB_freq_singlemodel_Description.csv"))
  }    
  
  write_csv(detailed_imp,
    paste0(results_dir,"Detailed_VI_XGB_freq_singlemodel.csv"))

  # generate variable importance for each fold of the CV:
  # for(i in 1:length(res$models)) {
  #   importance_fold <- xgb.importance(feature_names = res$models[[i]]$features,
  #                                     model = res$models[[i]]$learner.model)
  #   write_csv(importance_fold, paste0(importance_dir, "VI_XGB_freq_fold_", i, ".csv"))
  # }
}

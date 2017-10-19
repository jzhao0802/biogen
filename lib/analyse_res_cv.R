analyse_res_cv <- function(res, results_dir = "./") {
  # make pr curve:
  pr_curve <- perf_binned_perf_curve(pred = res$pred, bin_num = 20)
  
  write_csv(pr_curve$curve, 
    paste0(results_dir, "PRCurve_XGB_3fold_freq_100_bins.csv"))
  
  # ROCR pr curve:
  perf_vs_thresh <- generateThreshVsPerfData(res$pred, measures = list(tpr, ppv))
  
  plotROCCurves(perf_vs_thresh)
  ggsave(paste0(results_dir, "ROC_XGB_3fold.png"), plot = last_plot())
  
  #plot the pdf of the conditional scores
  res$pred$data = res$pred$data %>% mutate(logit = log(prob.1) - log(prob.0) )
    
  ggplot(res$pred$data) + aes(logit, group=truth, fill=truth) + 
    geom_density(alpha = .2) + xlab( 'logit scores' ) +
    scale_fill_discrete(name='discon.')

  fname = paste0(results_dir, "score_distributions.png")
  ggsave(fname, plot = last_plot())
  
  #ggplot(res$pred$data) + aes(x=) 
  #  geom_density(alpha = .2) + xlab( 'logit scores' ) +
  #  scale_fill_discrete(name='discon.') 
  
  write_csv(perf_vs_thresh$data, 
    paste0(results_dir, "ROCR_PRCurve_XGB_3fold_freq.csv"))
  
  # save the object
  saveRDS(res, paste0(results_dir, "res_XGB_3fold.rds"))
  
  #calculate auc
  roc_obj = pROC::roc(res$pred$data$truth, res$pred$data$prob.1)
  auc_param = pROC::auc(roc_obj)
  #write_csv(auc_param[1],paste0(results_dir, "auc.csv"))
  
  #calculate eer
  eer_ = calculate_eer(res$pred$data$prob.1, res$pred$data$truth)
  error_metric_ = tibble(auc = auc_param[1], eer = eer_)
  write_csv(error_metric_, paste0(results_dir, 'error_metric.csv'))
  
  # MEASURES ----------------------------------------------------------------
  write_csv(res$measures.test, paste0(results_dir, "test_error_XGB_3fold.csv"))
    
}
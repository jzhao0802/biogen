analyse_res_cv <- function(res, results_dir = "./") {
  # make pr curve:
  pr_curve <- perf_binned_perf_curve(pred = res$pred, bin_num = 100)
  
  write_csv(pr_curve$curve, 
    paste0(results_dir, "PRCurve_XGB_3fold_freq_100_bins.csv"))
  
  # ROCR pr curve:
  perf_vs_thresh <- generateThreshVsPerfData(res$pred, measures = list(tpr, ppv))
  
  plotROCCurves(perf_vs_thresh)
  ggsave(paste0(results_dir, "ROC_XGB_3fold.png"), plot = last_plot())
  
  write_csv(perf_vs_thresh$data, 
    paste0(results_dir, "ROCR_PRCurve_XGB_3fold_freq.csv"))
  saveRDS(res, paste0(results_dir, "res_XGB_3fold.rds"))
  
  # MEASURES ----------------------------------------------------------------
  write_csv(res$measures.test, paste0(results_dir, "test_error_XGB_3fold.csv"))
    
}
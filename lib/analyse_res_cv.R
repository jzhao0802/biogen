analyse_res_cv <- function(res, results_dir = "./", y_end=0.35) {
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
    
  ##------------
  # the function below has been replaced by cal_formatted_confusion_matrix.R
  neg = res$pred$data$logit [ res$pred$data$truth==0 ]
  pos = res$pred$data$logit [ res$pred$data$truth==1 ]
  
  #checking
  length(pos)
  length(neg)
  sum(res$pred$data$truth==1)
  sum(res$pred$data$truth==0)
  
  eer_ = calculate_eer_thrd(res$pred$data$logit, res$pred$data$truth)
  eer_prob = calculate_eer_thrd(res$pred$data$prob.1, res$pred$data$truth)
  
  true_pos = sum(pos > eer_$thrd)
  true_neg = sum(neg <= eer_$thrd)
  false_pos = sum(neg > eer_$thrd) #false alarm
  false_neg = sum(pos <= eer_$thrd) #false reject
  
  mat = matrix(0, 2,2)
  mat[1,1] = true_pos
  mat[2,2] = true_neg
  mat[1,2] = false_pos
  mat[2,1] = false_neg

  precision = true_pos /sum(mat[1,]) #or ppv
  recall = true_pos / sum(mat[,1])
  
  mat_ = as.data.frame(mat)
  rownames(mat_) = c('Predicted positive','Predicted negative')
  colnames(mat_) = c('Y=1','Y=0')
  
  mat_$rowname = rownames(mat_)
  mat_ = mat_ %>% select(one_of('rowname'),everything())
  rownames(mat_)= NULL
  
  
  mat_ = rbind(mat_,
               tibble(rowname='Precision', `Y=1`= precision, `Y=0`=NA),
               tibble(rowname='Recall', `Y=1`= recall, `Y=0`=NA),
               tibble(rowname='Threshold (logit)', `Y=1`= eer_$thrd, `Y=0`=NA),
               tibble(rowname='Threshold (Prob)', `Y=1`= eer_prob$thrd, `Y=0`=NA)
               )
  
  write_csv(mat_,paste0(results_dir,'confusion_matrix_atEER2.csv'))
  
  ##------------ new function
  mat_ = cal_formatted_confusion_matrix(pos,neg, eer_$thrd, is.prob.1 = FALSE )
  write_csv(mat_,paste0(results_dir,'confusion_matrix_atEER.csv'))
  
  # prec_func <- function(thrd, target_precision=0.75) {
  #   out_ = cal_formatted_confusion_matrix(pos,neg, thrd)
  #   prec_ = out_[3,2]
  #   return(abs(prec_- target_precision))
  # }
  #out = pracma::fminsearch(
  #  funtion(x) {prec_func(x, target_precision=0.75)}, 
  #  x0 = eer_$thrd, minimize = TRUE
  #  )
  #out = pracma::fminsearch(prec_func, c(-3,3), minimize = TRUE)
  #out = pracma::fminsearch(prec_func, c(eer_$thrd, 2), minimize = TRUE)

  ##------------ compute the confusion matrix at 75% precision
  # we use logit here:
  pred <- ROCR::prediction( res$pred$data$logit , res$pred$data$truth)
  
  perf <- ROCR::performance(pred,"ppv","rec")
  cutoffs <- data.frame(cut=perf@alpha.values[[1]], recall=perf@x.values[[1]], 
                        precision=perf@y.values[[1]])
  indice  = which.min(abs(cutoffs$precision-0.75))
    
  mat_ = cal_formatted_confusion_matrix(pos,neg,  
    cutoffs[indice,'cut'], is.prob.1 = FALSE )
  write_csv(mat_,paste0(results_dir,'confusion_matrix_at75precision.csv'))
  
  ##------------
  gg = ggplot(res$pred$data) + aes(logit, group=truth, fill=truth) + 
    geom_density(alpha = .2) + xlab( 'logit scores' ) +
    scale_fill_discrete(name='discon.')
  
  #plot the threshold
  gg + geom_segment(aes(x=eer_$thrd,y=0, xend=eer_$thrd, yend=y_end))
  
  fname = paste0(results_dir, "score_distributions_logit.png")
  ggsave(fname, plot = last_plot())
  
  #ggplot(res$pred$data) + aes(x=) 
  #  geom_density(alpha = .2) + xlab( 'logit scores' ) +
  #  scale_fill_discrete(name='discon.') 
  
  ##------------ plot probability distributions
  gg = ggplot(res$pred$data) + aes(prob.1, group=truth, fill=truth) + 
    geom_density(alpha = .2) + xlab( 'Prob scores' ) +
    scale_fill_discrete(name='discon.') + coord_cartesian(xlim = c(0, 1)) 
  
  #plot the threshold
  gg + geom_segment(aes(x=eer_$thrd,y=0, xend=eer_prob$thrd, yend=y_end))
  
  fname = paste0(results_dir, "score_distributions_prob.png")
  ggsave(fname, plot = last_plot())

    ##------------  
  write_csv(perf_vs_thresh$data, 
    paste0(results_dir, "ROCR_PRCurve_XGB_3fold_freq.csv"))
  
  # save the object
  saveRDS(res, paste0(results_dir, "res_XGB_3fold.rds"))
  
  #calculate auc
  roc_obj = pROC::roc(res$pred$data$truth, res$pred$data$prob.1, ci=TRUE)
  z=as.numeric(roc_obj$ci)
  #auc_param = pROC::auc(roc_obj)
  #write_csv(auc_param[1],paste0(results_dir, "auc.csv"))
  
  #calculate eer
  eer_ = calculate_eer(res$pred$data$prob.1, res$pred$data$truth)
  
  error_metric_ = tibble(auc.lower = z[1], auc.expected = z[2], 
    auc.upper=z[3], eer = eer_)
  write_csv(error_metric_, paste0(results_dir, 'error_metric.csv'))
  
  # MEASURES ----------------------------------------------------------------
  write_csv(res$measures.test, paste0(results_dir, "test_error_XGB_3fold.csv"))
    
}

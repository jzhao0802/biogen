calculate_eer_thrd <- function(scores, truth) {
  pred <- ROCR::prediction( scores, truth)
  
  perf <- ROCR::performance(pred,"tpr","fpr")
  cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                        tpr=perf@y.values[[1]])
  
  #plot(1-cutoffs$tpr,cutoffs$fpr )
  err_diff = abs(1-cutoffs$tpr -cutoffs$fpr)
  index_min = which.min(err_diff)
  #err_diff[index_min]

  #equal error rate
  eer_ = mean(c( 1-cutoffs$tpr[index_min], cutoffs$fpr[index_min]))
  return(list(eer=eer_, thrd=cutoffs$cut[index_min]))
}

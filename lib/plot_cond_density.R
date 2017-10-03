plot_cond_density <- function(data, var_index, target, target_label = 'outcome', description ='', alpha_cut_off=0.95, rm.zero=FALSE ) {
   
  
  #num_ = as.matrix( data[,var_index])
  num_ = data[, var_index]
  
  if (rm.zero){
    num__ = num_[num_!=0]
  } else {
    num__ = num_
  }
  q_ = quantile(num__, 
                probs = c(0.05, 0.5, alpha_cut_off), na.rm=TRUE)
  
  if (rm.zero) {
    selected_ = num_<=q_[3] & !is.na(num_) &!near(num_, 0)
    
  } else {
    selected_ = num_<=q_[3] & !is.na(num_)
  }
  
  #( sprintf("Var name: %s", combined_config$Column[var_index]) )
  #( sprintf("Description: %s", description_) )
  
  num_ = as.numeric(num_[selected_])
  label_ = as.factor(target[selected_])
  n_pos = sum(label_==1)
  n_neg = sum(label_==0)
  
  #title_ = sprintf("#pos = %d, #neg = %d, EER = %0.3f", n_pos, n_neg, eer_)
  title_ = sprintf("#pos = %d, #neg = %d", n_pos, n_neg)
  
  
  # calculate error but just before that do some checking to prevent error
  #1. check the total sample available
  if (length(num_) == 0) {
    return( list (ggplot = NA, pos = n_pos, neg = n_neg, eer = NA))
  }
  
  # do the plotting
  gg = ggplot() + aes(num_, group=label_,
                      fill=label_) + 
    geom_density(alpha = .2) + xlab( description ) +         
    scale_fill_discrete(name=target_label) +
    ggtitle(title_)
  
  print(gg)
  
  #2. If either n_neg or n_pos is zero (and the total sample is more than zero), 
  #   than EER is in theory 0%
  if ( min( c(n_neg,n_pos) ) == 0 ) {
    
    title_ = sprintf("#pos = %d, #neg = %d, EER = %0.3f", n_pos, n_neg, 0)
    gg = ggplot() + aes(num_, group=label_,
                        fill=label_) + 
      geom_density(alpha = .2) + xlab( description ) +         
      scale_fill_discrete(name=target_label) +
      ggtitle(title_)
    print(gg)
    
    return( list (ggplot = gg, pos = n_pos, neg = n_neg, eer = 0))
  } 
  
  # attempt to calculate eer one way but if not (i.e.,eer < 50%) then another
  eer_ = calculate_eer(num_,label_)
  if (eer_ > 0.5) {
    eer_ = calculate_eer(-num_,label_)
  }
  title_ = sprintf("#pos = %d, #neg = %d, EER = %0.3f", n_pos, n_neg, eer_)
  gg = gg + ggtitle(title_)
  print(gg)
  return( list (ggplot = gg, pos = n_pos, neg = n_neg, eer = eer_))
}


calculate_eer <- function(num_, label_) {
  pred <- prediction( num_, label_)
  
  perf <- performance(pred,"tpr","fpr")
  cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                        tpr=perf@y.values[[1]])
  
  #plot(1-cutoffs$tpr,cutoffs$fpr )
  err_diff = abs(1-cutoffs$tpr -cutoffs$fpr)
  index_min = which.min(err_diff)
  #err_diff[index_min]
  #equal error rate
  eer_ = mean(c( 1-cutoffs$tpr[index_min], cutoffs$fpr[index_min]))
  return(eer_)
}


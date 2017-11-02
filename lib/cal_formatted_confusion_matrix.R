cal_formatted_confusion_matrix <- function(pos, neg, thrd, is.prob.1=TRUE) {
  #pos is a list of positive scores
  #neg is a list of negative scores
  #thrd is the decision threshold to use
  
  #return a matrix 
  true_pos = sum(pos > thrd)
  true_neg = sum(neg <= thrd)
  false_pos = sum(neg > thrd) #false alarm
  false_neg = sum(pos <= thrd) #false reject
  
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

  logit <- function(x) {
    return( log (x/(1-x)))
  }
  
  sigmoid <- function(x) {
    return (1/(1+exp(-x) ))
  }
  if (is.prob.1) {
    mat_ = rbind(mat_,
                 tibble(rowname='Precision', `Y=1`= precision, `Y=0`=NA),
                 tibble(rowname='Recall', `Y=1`= recall, `Y=0`=NA),
                 tibble(rowname='Threshold (Prob)', `Y=1`= thrd, `Y=0`=NA),
                 tibble(rowname='Threshold (logit)', `Y=1`= logit(thrd), `Y=0`=NA)
    )
  } else { #we are receiving logit scores here; so turn it to prob
    mat_ = rbind(mat_,
                 tibble(rowname='Precision', `Y=1`= precision, `Y=0`=NA),
                 tibble(rowname='Recall', `Y=1`= recall, `Y=0`=NA),
                 tibble(rowname='Threshold (logit)', `Y=1`= thrd, `Y=0`=NA),
                 tibble(rowname='Threshold (Prob)', `Y=1`= sigmoid(thrd), `Y=0`=NA)
    )
  }  
  return(mat_)
}

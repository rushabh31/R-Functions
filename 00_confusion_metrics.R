conf_metrics <- function(model, actual_class, pred_risk, cutoff = 0.1) {
  
  if(!require("pROC")){install.packages("pROC"); library(pROC)}
  if(!require("PRROC")){install.packages("PRROC"); library(PRROC)}
  if(!require("purrr")){install.packages("purrr"); library(purrr)}
  
  
  
  if (max(pred_risk) > cutoff) {
       
    pred_class <- ifelse(pred_risk >= cutoff, 1, 0)
    
    pred_risk %>% pROC::roc(actual_class, .) %>% pROC::auc(.) -> auc_test
    
    pred_risk_pos <- pred_risk[actual_class == 1]
    pred_risk_neg <- pred_risk[actual_class == 0]
    
    pr <- PRROC::pr.curve(scores.class0 = pred_risk_pos, scores.class1 = pred_risk_neg, curve = T)
    prauc_test <- pr$auc.integral
    
    outcome <- c()
    # Confusion matrix and definition of each cell
    conf <- table(actual_class, pred_class)
    TP <- conf[2,2]
    TN <- conf[1,1]
    FP <- conf[1,2]
    FN <- conf[2,1]
    
    ACC <- sum(diag(conf))/sum(conf)
    TPR <- TP /(TP + FN) #same as recall
    FPR <- FP /(FP + TN)
    SPEC <- 1 - (FP /(FP +TN))
    PREC <- TP /(TP+FP) 
     
    1:3 %>%
      map(function(beta) ((1+beta^2)*PREC*TPR)/((beta^2*PREC+TPR))) -> F_stat
    
    return(list( CUTOFF = cutoff,AUC = auc_test, AUPR = prauc_test, confusion_matrix = conf, TP = TP, TN = TN, FP = FP, FN = FN, Accuracy = ACC, TPR = TPR, FPR = FPR, Specificity = SPEC, Precision = PREC, F1 = F_stat[[1]], F2 = F_stat[[2]], F3 = F_stat[[3]]))
  } else {
    return(list( CUTOFF = cutoff,AUC = NA, AUPR = NA, confusion_matrix = NA, TP = NA, TN = NA, FP = NA, FN = NA, Accuracy = NA, TPR = NA, FPR = NA, Specificity = NA, Precision = NA, F1 = NA, F2 = NA, F3 =NA))}
}

compare_models <- function(models = NULL,
                           n_var = 10,
                           actual_class=NULL,
                           risk_scores= NULL,
                           cutoffs = c(0.1, 1)
){
  
  #set up
  n_cutoff = length(cutoffs)
  comparison_output = c()
  importance_file <- xgboost::xgb.importance(feature_names = models$feature_names, model = models)
  data.table::setDT(importance_file)[,X:=1:.N]
  importance_file <- as.data.frame(importance_file)
  
  for (i in 1:n_cutoff) {
    
    output <- conf_metrics(model = model, actual_class = actual_class, pred_risk = risk_scores , cutoff = cutoffs[i])
    
    comp_out <- data.frame(model_nbr = i,
                           output[- which(names(output) %in% "confusion_matrix")],
                           dcast(importance_file[1:n_var,], 1 ~ paste("IMP_VAR_", X, sep = ""), value.var = "Feature"),
                           dcast(importance_file[1:n_var,], 1 ~ paste("GAIN_", X, sep = ""), value.var = "Gain"),
                           gain_top10 = sum(importance_file[1:n_var, c("Gain")])
    )
    
    comparison_output <- rbind(comparison_output, comp_out)
    
  }
  
  comparison_output[which(!names(comparison_output)%in%c('X1','X1.1'))]
  comparison_output
}

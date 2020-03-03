aucpr <- function(model,data,labels){
  require(PRROC)
  pred <- data.frame(prediction= predict(model, data),label=labels)
  fg <- pred[pred$label == 1,"prediction"]
  bg <- pred[pred$label == 0,"prediction"]
  
  
  # ROC Curve    
  roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  plot(roc)
  
  # PR Curve
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  plot(pr)
  
  return(list(roc,pr))
}

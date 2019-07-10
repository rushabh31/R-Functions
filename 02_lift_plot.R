# 
# lift <- function(actual_outcome, risk_propensity, groups=100) {
#   if(!require(dplyr)){
#     install.packages("dplyr")
#     library(dplyr)}
#   if(is.factor(actual_outcome)) actual_outcome <- as.integer(as.character(actual_outcome))
#   if(is.factor(risk_propensity)) risk_propensity <- as.integer(as.character(risk_propensity))
#   
#   helper = data.frame(cbind(actual_outcome, risk_propensity))
#   helper[,"bucket"] = ntile(-helper[,"risk_propensity"], groups)
#   
#   gaintable = helper %>% group_by(bucket)  %>%
#     summarise_at(vars(actual_outcome), funs(total = n(),
#                                     total_positives=sum(., na.rm = TRUE))) %>%
#     mutate(cumm_positives = cumsum(total_positives),
#            gain=cumm_positives/sum(total_positives)*100,
#            cumm_lift=gain/(bucket*(100/groups)))
#   return(gaintable)
# }




lift <- function(actual_outcome,risk_propensity,groups=100){
  if(!require(data.table)){
    install.packages("data.table")
    library(data.table)}

  if(is.factor(actual_outcome)) actual_outcome <- as.integer(as.character(actual_outcome))
  if(is.factor(risk_propensity)) risk_propensity <- as.integer(as.character(risk_propensity))
  dt = data.table(cbind(actual_outcome, risk_propensity))

  #Order the people according to rank Propensity
  setorder(dt,-risk_propensity)
  #Calculate the percentage of total responses for each cutoff point
  
  
  ## Response Rate = Number of Responses  / Total Number of Responses (10)
  dt[,bucket:=ntile(-risk_propensity, groups)]
  gaintable <- dt[, .(total = .N, total_positives = sum(actual_outcome, na.rm = TRUE)), by = bucket]
  gaintable[, `:=` (total_cumsum = cumsum(total),
                    cumsum_positives = cumsum(total_positives),
                    gain = (cumsum(total_positives)/sum(total_positives)*100),
                    cumlift=(cumsum(total_positives)/sum(total_positives)*100)/(bucket*(100/groups)),
                    total_cumsum_pos_perc = cumsum(total_positives)/sum(total_positives)*100 ,
                    total_cumsum_perc = cumsum(total)/sum(total)*100,
                    baseline_perc = cumsum(total)/sum(total)*100)]
  
  return(gaintable)

}

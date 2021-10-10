#' function adding any aggragated variables of interest
#' @param data data set with subject, moodtracker, task and questinnaire data
#' @export
addSummaryVariables <- function(data){
  
  before <- nrow(data) # how many entries do we have before?
  

  # add new columns: 
  # --> if new aggregated variables are added, make sure to complete them her as well
  aggVariables <- c("completedTask", "NTasksSession", "firstUse", "lastUse",  "TimesUsageTotal", "NTasks", 
                    "N_2Back","N_affectiveSetShifting", "N_backwardDigitSpan", "N_emotionalStroop","N_nBack" )
  data[aggVariables] <- NA
  
  data$rowGlobal <- 1:nrow(data)
  
  for(sub in unique(data$subNo)){
    
    
    # break if unidentifiable subject.. we delete this later anyways and it makes everything break
    if(sub != "unidentifiable"){
      subData <- data[data$subNo == sub,]
      
      
      
      # Did participant complete ANY task at this timepoint?
      #check if any of the "completedTask" variables is TRUE (=1) --> if yes, mark as true
      # first 5 "completed" variables are from tasks --> we don't want to count questionnaire completions
      data$completedTask[subData$rowGlobal] <- ifelse(rowSums(dplyr::select(subData, dplyr::contains("completed"))[1:5]) > 0, T, F)
      
      # How many tasks did participant complete in one session?
      data$NTasksSession[subData$rowGlobal] <- rowSums(dplyr::select(subData, dplyr::contains("completed"))[1:5])
      
      #How many times participants used the app (any usage)
      data$TimesUsageTotal[subData$rowGlobal] <- nrow(subData)
      
      
      #How many times participants completed any task
      data$NTasks[subData$rowGlobal] <- sum(rowSums(dplyr::select(subData, dplyr::contains("completed"))[1:5]))
      
      #How many times participants completed the ucl_2back
      data$N_2Back[subData$rowGlobal] <- sum(subData$`2Back_completed`)
      
      #How many times participants completed the ucl_affectiveSetShifting
      data$N_affectiveSetShifting[subData$rowGlobal] <- sum(subData$affectiveSetShifting_completed)
      
      #How many times participants completed the ucl_backwardDigitSpan
      data$N_backwardDigitSpan[subData$rowGlobal] <- sum(subData$backwardDigitSpan_completed)
      
      #How many times participants completed the ucl_emotionalStroop
      data$N_emotionalStroop[subData$rowGlobal] <- sum(subData$emotionalStroop_completed)
      
      #How many times participants completed the ucl_nBack
      data$N_nBack[subData$rowGlobal] <- sum(subData$nBack_completed)
      
      #Date first & last use
      TimeStamps <- as.vector(as.matrix(dplyr::select(subData, dplyr::contains("timeStamp")))) # put all timestamps into one vector
      data$firstUse[subData$rowGlobal] <- min(TimeStamps, na.rm = T) # first use (smallest timestamp)
      data$lastUse[subData$rowGlobal] <- max(TimeStamps, na.rm = T) # last use (smallest timestamp)
      
      
    } # end if
  } # emd for
  
  
  
  data$subNo <- as.numeric(data$subNo)
  
  data <- data %>%
    dplyr::select(c(subNo, subID, study_id, moodTracker_timeStamp, dplyr::all_of(aggVariables)), dplyr::everything()) %>% # move new columns to front
    dplyr::filter(subID != "unidentifiable") %>% # get rid of all unidentifiable data points 
    dplyr::arrange(subNo)
  
  
  
  removed <- before - nrow(data)
  print("---------------------------------------")
  print("Removing duplicated entries and unidentifiable subjects")
  print(paste( "--->" , removed, "data entires removed"))
  
  
  
  return(data)
  
}






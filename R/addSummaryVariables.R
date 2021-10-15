#' function adding any aggragated variables of interest
#' @param data data set with subject, moodtracker, task and questinnaire data
#' @export
addSummaryVariables <- function(data){
  
  #before <- nrow(data) # how many entries do we have before?
  

  # --> we included 10 repetitions of each moodtracker to fill up instances where subject did same task twice
  # --> we need to remove empty rows now
  
  # delete empty rows
  
  # add session counter
  

  
  # -------------------------- # 
  # 2: add summary variables
  # -------------------------- # 
  # add new columns: 
  # --> if new aggregated variables are added, make sure to complete them her as well
  aggVariables <- c("SessionCounter", "completedTask", "NTasksSession", "firstUse", "lastUse",  "TimesUsageTotal", "NTasks", 
                    "N_2Back","N_affectiveSetShifting", "N_backwardDigitSpan", "N_emotionalStroop","N_nBack" )
  data[aggVariables] <- NA
  
  # adjust rowGlobal --> we need it to access data set 
  data$rowGlobal <- 1:nrow(data)
  deleteRowNumbers <- c() # prepare to delete empty rows later
  
  for(sub in unique(data$subNo)){
    
    
    # break if unidentifiable subject.. we delete this later anyways and it makes everything break
    if(sub != "unidentifiable"){
      subData <- data[data$subNo == sub,]
      
      # ----- add session counter ------- #
      sessions <- nrow(subData)/10 # how many times did subject use app/moodtracker?
      data$SessionCounter[subData$rowGlobal] <- sort(rep(1:sessions, 10))
      subData$SessionCounter <- sort(rep(1:sessions, 10))
      
      
      # -------------------------- # 
      # 1: summarize each session
      # -------------------------- # 
      # summarize each session
      for(sess in unique(subData$SessionCounter)){
        
        sessData <- dplyr::filter(subData, SessionCounter == sess)
        
        # Did participant complete ANY task at this timepoint/session?
        #check if any of the "completedTask" variables is TRUE (=1) --> if yes, mark as true
        # first 5 "completed" variables are from tasks --> we don't want to count questionnaire completions
        didTask <- ifelse(rowSums(dplyr::select(sessData, dplyr::contains("completed"))[1:5]) > 0, T, F)
        data$completedTask[sessData$rowGlobal] <- didTask[1]
      
        # How many tasks did participant complete in one session?
        data$NTasksSession[sessData$rowGlobal] <- sum(rowSums(dplyr::select(sessData, dplyr::contains("completed"))[1:5]))
        
        
        # prepare to delete empty rows later
        if(!is.na(match(F, didTask))){
          
          if(sum(didTask) == 0){ # no task was done at all
            emptyRows <- 2:10 # keep first row!
            delete <- sessData$rowGlobal[emptyRows]
          }else{
            emptyRows <- match(F, didTask):10
            delete <- sessData$rowGlobal[emptyRows]
          }
        } else {
          delete <- NA
          print("10 or more")
          print(sub)
        }
        
        # store row numbers that we will delete later
        deleteRowNumbers <- c(deleteRowNumbers, delete)
        
      }
      

      
      # -------------------------- # 
      # 1: summarize each subject
      # -------------------------- # 
      #How many times participants used the app (any usage)
      data$TimesUsageTotal[subData$rowGlobal] <- sessions
      
      
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
  } # end for
  
  # delete empty row numbers
  deleteRowNumbers <- deleteRowNumbers[!is.na(deleteRowNumbers)]
  data <- data[!(data$rowGlobal %in% deleteRowNumbers),]
  
  #data$subNo <- as.numeric(data$subNo)
  before <- nrow(data)
  
  
  data <- data %>%
    dplyr::select(c(subNo, subID, study_id, SessionCounter, TaskCounter, moodTracker_timeStamp, dplyr::all_of(aggVariables)), dplyr::everything()) %>% # move new columns to front
    dplyr::filter(subID != "unidentifiable")# %>% # get rid of all unidentifiable data points 
   
  
  
  removed <- before - nrow(data)
  print("---------------------------------------")
  print("Removing unidentifiable subjects")
  print(paste( "--->" , removed, "data entires removed"))
  
  
  
  return(data)
  
}






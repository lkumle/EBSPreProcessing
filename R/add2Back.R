add2Back <- function(data, BACK2, subDatabase){
  
  # ------------------------------------------ # 
  # 0: prepare extension of data frame 
  # ------------------------------------------ #
  
  data$rowGobal <- 1:nrow(data) # variable for indexing
  data$TP2BACK <- NA
  
  
  data$Completed2Back <- FALSE # default of false (change to TRUE if 2BACK task was completed)
  
  # add columns to store 2BACK data 
  B2_columns <- names(BACK2)[30:242]
  data[B2_columns] <- NA
  
  # ------------------------------------------ # 
  # 1: go through all etries in mood data file
  # ------------------------------------------ #
  for(tp in 1:nrow(BACK2)){
    
    # ------------------------------- # 
    # 2: match with subject DATA
    # ------------------------------- #
    
    subINFO <- BACK2[tp, c("study_id", "deviceModel", "deviceUUID")]
    
    # B: match with entry in subDatabase and retrieve subject data
    subID <- matchSubID(subINFO, subDatabase)
    
    if(is.na(subID)){ # if no entry in subject Database: subset NA ro
      subID <- "unidentifiable"}
    
    # ------------------------------- # 
    # 3: match with timeStamp data
    # ------------------------------- #
    
    # A: get all moodTracker entries of present subject:
    SubMoodTP <- data[data$subID == subID,] 
    
    # B: compare timestamps
    tp2Back <- BACK2$timeStamp[tp] # timestamp 2BAck
    
    # compare timestamp of 2Back task with all timestamps of mood data 
    # --> get closet one in the future (only positive dates )
    # --> return index to  mark session 
    timeDiffs <- difftime(tp2Back, SubMoodTP$timeStamp)
    closest <- which.min(timeDiffs[timeDiffs > 0])
    session <- which(timeDiffs == timeDiffs[timeDiffs > 0][closest])
    
    row <- SubMoodTP$rowGobal[session] # which global row does this session correspond to?
    
    # C: add data  (TRUE <- value for completed 2BACK)
    task_data <- c(TRUE, BACK2[tp,30:242])
    
    data$TP2BACK[row] <- as.POSIXct(tp2Back)
    data[row, 21:234] <- task_data
    
  }
  
  data$TP2BACK <- as.POSIXct(data$TP2BACK, origin = "1970-01-01", tz = 'GMT')
  
  return(data)
  
}

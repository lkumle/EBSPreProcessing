# function matching timepoint of task data with moodtracker
matchTimepoint <- function(data, tp , subID, taskData){
  
  # A: get all moodTracker entries of present subject:
  SubMoodTP <- data[data$subID == subID,] 
  
  # B: compare timestamps
  tpTask <- taskData$timeStamp[tp] # timestamp 2BAck
  
  # compare timestamp of 2Back task with all timestamps of mood data 
  # --> get closet one in the future (only positive dates)
  # --> return index to  mark session 
  timeDiffs <- difftime(tpTask, SubMoodTP$timeStamp)
  closest <- which.min(timeDiffs[timeDiffs > 0])
  session <- which(timeDiffs == timeDiffs[timeDiffs > 0][closest])
  
  row <- SubMoodTP$rowGobal[session] # which global row does this session correspond to?
  
  return(row)
}
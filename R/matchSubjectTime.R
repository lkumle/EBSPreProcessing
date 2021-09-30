# Functions in this file: 
# 1: matchSubID -> find subject ID for subject of interest
# 2: matchTimepoint -> find closest timepoint of subject in moodtracker data


#' Function for matching subject data with information stored in subject database
#' Returns subI  unique subID from database
#' @param subINFO c("study_id", "deviceModel", "deviceUUID") of subject we want to match
#' @param subDatabase database containing subject data for each subject
#' @export
matchSubID <- function(subINFO, subDatabase){
  
  # vectorize data
  row_strings <- do.call(paste0, subDatabase[c("study_id", "deviceModel", "deviceUUID")])
  check_strings <- paste0(subINFO, collapse = "")
  
  # use match to get first match
  idx <- match(check_strings, row_strings)
  
  # use index to retrieve subID
  subID <- subDatabase$subID[idx]
  
  return(subID)
}

# ------------------------------------------------------------- #

#' function matching timepoint of task data with moodtracker
#' @param data data with subject info and moodtracker (+ task/questionnaie data)
#' @param tp timepoint (row number) in task/questionnaire data
#' @param subID subject identifiication (returned from matchSubID)
#' @param taskData task/questionnaire data 
#' 
matchTimepoint <- function(data, tp , subID, taskData){
  
  # A: get all moodTracker entries of present subject:
  SubMoodTP <- data[data$subID == subID,] 
  
  # B: compare timestamps
  tpTask <- taskData$timeStamp[tp] # timestamp 2BAck
  
  # compare timestamp oftask with all timestamps of mood data 
  # --> get closet one in the future (only positive dates)
  # --> return index to  mark session 
  timeDiffs <- difftime(tpTask, SubMoodTP$moodTracker_timeStamp)
  closest <- which.min(timeDiffs[timeDiffs > 0])
  session <- which(timeDiffs == timeDiffs[timeDiffs > 0][closest])
  
  row <- SubMoodTP$rowGlobal[session] # which global row does this session correspond to?
  
  return(row)
}


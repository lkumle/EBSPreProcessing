#' Function for adding Questionnaire data 
#' --> assumes moodtracer and subject data has already been combined
#' 
#' @param data Preprocessed data frame that questionnaire data shuld be added to
#' @param subDatabase  Data frame containing subject information. Used to match data and subject
#' @param taskID name of task/questionnaire that should be added
#' @param taskData data frame containing task/questionnare data
#' @param Q add questionnaire data? set to true if yes
#' @export

addTaskData <- function(data,subDatabase, taskID, taskData, Q = F){
  
  # ------------------------------------------ # 
  # 0: prepare extension of data frame 
  # ------------------------------------------ #
  print(paste("Task:", taskID)) # notify which data file is being processed right now
  
  # AA: convert timeStamp in taskData for matching with moodtracker timeStamp
  taskData$timeStamp <- as.POSIXct(taskData$timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  
  # A:add timestamp  and variable if task was completed at this timestamp (TRUE/FALSE)
  data[paste0(taskID, "_timeStamp" )] <- as.POSIXct(NA) 
  data[paste0(taskID, "_completed")] <- FALSE # default of false (change to TRUE if task task was completed)
  
  # B: check how many colums data frame has by now (to know where to start indexing data later)
  colsBefore <- ncol(data) + 1 #new coumns should start one colum next to last column
  

  
  # C:rename variables so we know where they came from
  # --> addtask name at beginning of all columns
  oldnames <- names(taskData[30:ncol(taskData)])
  newnames <- paste0(taskID, "_", oldnames)
  taskData <- dplyr::rename_with(taskData, ~ newnames[which(oldnames == .x)], .cols = dplyr::all_of(oldnames))
  
  # D: get last variable of interest
  if(Q == T){
    # questionaire data has some empty columns at the end --> we don't want them in our processed data frame
    # --> last column is one column before (-1) first columns that is empty
    lastVariable <- match("<undefined>", taskData[1,]) -1
  } else{
    lastVariable <- ncol(taskData)  
  }
  
  task_columns <- names(taskData)[30:lastVariable] # use indexes provided
  data[task_columns] <- NA
  
  colsAfter <- ncol(data) # check how many columns data frame has now
  
  sumNoMatch <- 0
  # ------------------------------------------ # 
  # 1: go through all etries in task data file
  # ------------------------------------------ #
  for(tp in 1:nrow(taskData)){
    
    # ------------------------------- # 
    # 2: match with subject DATA
    # ------------------------------- #
    
    subINFO <- taskData[tp, c("study_id", "deviceModel", "deviceUUID")]
    
    # B: match with entry in subDatabase and retrieve subject data
    subID <- matchSubID(subINFO, subDatabase)
    
    if(is.na(subID)){ # if no entry in subject Database: subset NA ro
      subID <- "unidentifiable"}
    
    # ------------------------------- # 
    # 3: match with timeStamp data
    # ------------------------------- #
    
    # find row in data that is clostest to timepoint of task data
    row <- matchTimepoint(data, tp, subID, taskData)
    
    # count how many entries could not be matched
    if(length(row) == 0){sumNoMatch <- sumNoMatch + 1}
    
    # C: add data  
    data[row, paste0(taskID, "_timeStamp")] <- taskData$timeStamp[tp] # store timeStamp of task completed
    data[row, paste0(taskID, "_completed")] <- T # set completed task to True
    
    # add data from task to data frame
    tp_data_task <- c(taskData[tp,30:lastVariable]) # get relevant variables of task data ()
    data[row, colsBefore:colsAfter] <- tp_data_task # add to corresponding row in data (using column boundaries determined earlier)
    
    
  } # end tp
  
  #data[row, paste0("timeStamp", taskID)] <- as.POSIXct(data[paste0("timeStamp", taskID)],  format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  print(paste(sumNoMatch, "data point(s) failed to match"))
  return(data)
} # emd function

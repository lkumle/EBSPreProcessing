# create generic "add task function"
# data = preprocessed data set
# subDatabase = subject database used for matching subjects
# taskID = name of task (used to create variable names)
# taskData = data file of task we want to add
# idx = vector with indexes specifying variables in task data we want to add

addTaskData <- function(data, subDatabase,taskID, taskData){
  
  # ------------------------------------------ # 
  # 0: prepare extension of data frame 
  # ------------------------------------------ #
  print(paste("Task:", taskID)) # notify which data file is being processed right now
  
  # AA: convert timeStamp in taskData for matching with moodtracker timeStamp
  taskData$timeStamp <- as.POSIXct(taskData$timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  
  # A:add timestamp  and variable if task was completed at this timestamp (TRUE/FALSE)
  data[paste0("timeStamp", taskID)] <- as.POSIXct(NA) 
  data[paste0("completed", taskID)] <- FALSE # default of false (change to TRUE if task task was completed)
  
  # B: check how many colums data frame has by now (to know where to start indexing data later)
  colsBefore <- ncol(data) + 1 #new coumns should start one colum next to last column
  
  # C:get column names of task data
  
  # how many variables are we cutting off at the  end? all that start with pres + progVar
  taskData <-  taskData[ , ! names(taskData) %in% c("presOrder_1", "presOrder_2", "presOrder_3", "presOrder_4", "presOrder_5", "progVar")] 
  
  # deal with variable names that occur more than once: (manually extracted - might be better automated)
  if("immMoodScore" %in% names(taskData)){
    names(taskData)[names(taskData)=="immMoodScore"] <- paste(taskID, "_immMoodScore")}
  if("dPrimeNegEmo" %in% names(taskData)){
    names(taskData)[names(taskData)=="dPrimeNegEmo"] <- paste(taskID, "dPrimeNegEmo")}
  if("falseAlarmsNegEmo" %in% names(taskData)){
    names(taskData)[names(taskData)=="falseAlarmsNegEmo"] <- paste(taskID, "falseAlarmsNegEmo")
  }
  
  
  lastVariable <- ncol(taskData) #- (sum(grepl("^pres", names(taskData))) +sum(grepl("^prog", names(taskData)))) 
  task_columns <- names(taskData)[30:lastVariable] # use indexes provided
  data[task_columns] <- NA
  
  colsAfter <- ncol(data) # check how many columns data frame has now
  
  sumNoMatch <- 0
  # ------------------------------------------ # 
  # 1: go through all etries in mood data file
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
    data[row, paste0("timeStamp", taskID)] <- taskData$timeStamp[tp] # store timeStamp of task completed
    data[row, paste0("completed", taskID)] <- T # set completed task to True
    
    # add data from task to data frame
    tp_data_task <- c(taskData[tp,30:lastVariable]) # get relevant variables of task data ()
    data[row, colsBefore:colsAfter] <- tp_data_task # add to corresponding row in data (using column boundaries determined earlier)
    
    
  } # end tp
  
  #data[row, paste0("timeStamp", taskID)] <- as.POSIXct(data[paste0("timeStamp", taskID)],  format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  print(paste(sumNoMatch, "data point(s) failed to match"))
  return(data)
} # emd function

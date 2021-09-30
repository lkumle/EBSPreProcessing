#' function combining data from introductiory questionnaire with data from moodtracker
#' Returns data frame where one row == one time moodtracker was completed
#' @param MOOD data frame containing moodtracker data
#' @param subDatabase data base with INTRO questionnaire data
#' @export
addMoodtracker <- function(MOOD, subDatabase){
  
  print("Match Moodtracker data to subject IDs")
  # create data frame
  data <- data.frame()
  
  # rename variables (so we know where they are coming from later!)
  oldnames <- c(names(MOOD[30:ncol(MOOD)]), "timeStamp")
  newnames <- paste0("moodTracker_", oldnames)
  MOOD <- rename_with(MOOD, ~ newnames[which(oldnames == .x)], .cols = all_of(oldnames))
  
  # convert timestamp
  MOOD$moodTracker_timeStamp <- as.POSIXct(MOOD$moodTracker_timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  
  
  
  # count how many entries can not be matched
  sumNoMatch <- 0
  # ------------------------------------------ # 
  # 1: go through all etries in mood data file
  # ------------------------------------------ #
  for(sess in 1:nrow(MOOD)){
    
    # ------------------------------- # 
    # 2: match with subject DATA
    # ------------------------------- #
    # A: get subject indetification variables
    subINFO <- MOOD[sess, c("study_id", "deviceModel", "deviceUUID")]
    
    # B: match with entry in subDatabase and retrieve subject data
    subID <- matchSubID(subINFO, subDatabase)
    
    if(is.na(subID)){ # if no entry in subject Database: subset NA ro
      subID <- "unidentifiable" 
      sumNoMatch <- sumNoMatch+ 1}
  
    # retrieve data (we can cut out the device/name variables - from here on we will use subject numbers)
    IDData <- subDatabase[subDatabase$subID == subID, 4:ncol(subDatabase)]  # subject data
    moodData <- dplyr::select(MOOD[sess,], c(moodTracker_timeStamp, moodTracker_R1_R:moodTracker_R4_RT)) # moodtracker data
    
    # rename moodtracker data (so we know wher the variables come from later)
  
    # C: add to main data file
    sessDat <- c(IDData, moodData) # combine with mood data
    data <- rbind(data, sessDat)
    
    data$moodTracker_timeStamp <- as.POSIXct(data$moodTracker_timeStamp, origin = '01-01-1970',tz='GMT')
    data$INTRO_timeStamp <- as.POSIXct( data$INTRO_timeStamp, origin = '01-01-1970',tz='GMT')
    
  } #end for ID in unique ID

  # return data frame
  data$rowGlobal <- 1:nrow(data) # variable for indexing later on
  
  # convert to posixct again....
  
  # reorder data to have subNO and subID at the front: 
  data <- data %>%
    select(rowGlobal, subNo, subID, everything())
  
  print(paste(sumNoMatch, "data point(s) failed to match"))
  
  return(data)
}

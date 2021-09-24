addMoodtracker <- function(MOOD, subDatabase){
  
  
  # create data frame
  data <- data.frame()
  
  MOOD$timeStamp <- as.POSIXct(MOOD$timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
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
      subID <- "unidentifiable" }
  
    IDData <- subDatabase[subDatabase$subID == subID,] # retrieve data
  
    # C: add to main data file
    sessDat <- c(IDData, dplyr::select(MOOD[sess,], c(timeStamp, R1_R:R4_RT))) # combine with mood data
    data <- rbind(data, sessDat)
    
    data$timeStamp <- as.POSIXct( data$timeStamp, origin = '01-01-1970',tz='GMT')
    
  } #end for ID in unique ID

  # return data frame
  data$rowGobal <- 1:nrow(data) # variable for indexing later on
  
  return(data)
}

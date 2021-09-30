#' Function for creating dataframe containing unique subject information-
#' Used as reference to match participants data across frames
#' @param INTRO data frame containing data from introductory questionnaire
#' @export
 
createDatabase <- function(INTRO){
  
  # convert timestamp 
  INTRO$timeStamp <- as.POSIXct(INTRO$timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')
  
  # get dataframe with entries unique on username (study_id) and device information 
  uniqueID <- INTRO[!duplicated(INTRO[, c("study_id", "deviceModel", "deviceUUID")]),]
  
  # only keep variables that we need in the preprocessed data frame
  uniqueID <- dplyr::select(uniqueID, c(study_id, deviceModel, deviceUUID, timeStamp, R1_R:R11_RT))
  
  # rename variables (add INTRO_ ): moodtracker variables are also calles R1 etc (we don't want to mix them up!)
  uniqueID <- dplyr::rename_at(uniqueID, dplyr::vars(names(uniqueID[,4:37])), ~ paste0("INTRO_",names(uniqueID[,4:37])))
  
  # index users
  uniqueID$subNo <- c(1:nrow(uniqueID)) # numerical index
  uniqueID$subID <- paste0("sub", c(1:nrow(uniqueID)))
  
  # add NA row to index when subject not identifyable
  uniqueID <- rbind(uniqueID, c(rep(NA, 37), -1, "unidentifiable"))
  
  return(uniqueID)
  
} # end function






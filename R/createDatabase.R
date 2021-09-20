# Function for creating dataframe containing unique subject information-
# Used as reference to match participants data across frames
createDatabase <- function(file){
  
  # get dataframe with entries unique on username (study_id) and device information 
  uniqueID <- file[!duplicated(file[, c("study_id", "deviceModel", "deviceUUID")]), c("study_id", "deviceModel", "deviceUUID")]
  
  # index users
  uniqueID$subNo <- c(1:nrow(uniqueID)) # numerical index
  uniqueID$subID <- paste0("sub", c(1:nrow(uniqueID)))
  
  # add NA row to index when subject not identifyable
  uniqueID <- rbind(uniqueID, c(rep(NA, 3), 000, "unidentifiable"))
  
  return(uniqueID)
  
} # end function






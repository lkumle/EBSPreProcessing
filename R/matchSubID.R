#' Function for matching subject data with information stored in subject database
#' Returns subI  unique subID from database
matchSubID <- function(subINFO, subDatabase){
  
  # vectorize data
  row_strings <- do.call(paste0, subDatabase[1:3])
  check_strings <- paste0(subINFO, collapse = "")
  
  # use match to get first match
  idx <- match(check_strings, row_strings)
  
  # use index to retrieve subID
  subID <- subDatabase$subID[idx]
  
  return(subID)
}


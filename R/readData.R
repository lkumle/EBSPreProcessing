#' function reading in all data files and storing them in a list

readData <- function(path){
  
  files <- list.files(path, pattern = ".csv") # list all csv files specified in path 
  dataFrames<- lapply(paste0(path,files), read.csv) # read in all files
  
  # store name of file in list as well so we can find it later
  names(dataFrames) <- gsub("\\.csv$", "", files) # cut off the .csv part
  names(dataFrames) <- gsub(files, pattern = ".csv", replacement = "")
  
  return(dataFrames)
}

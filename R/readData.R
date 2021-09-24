# function reading in all data files

readData <- function(path){
  
  files <- list.files(path) # list all csv files specified in path 
  dataFrames<- lapply(paste0(path,files), read.csv) # read in all files
  
  # store name of file in list as well so we can find it later
  names(dataFrames) <- gsub("\\.csv$", "", files)
  names(dataFrames) <- gsub(files, pattern = ".csv", replacement = "")
  
  return(dataFrames)
}

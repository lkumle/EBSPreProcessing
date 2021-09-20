# function reading in all data files

readData <- function(path){
  
  INTRO <<- read.csv(paste0(path,"q_UCL_introQ.csv"))
  
  PANAS <<- read.csv(paste0(path,"/q_UCL_PANAS.csv"))
  
  SDQminus <<- read.csv(paste0(path,"q_UCL_SDQ17minus.csv"))
  
  SDQplus <<- read.csv(paste0(path,"q_UCL_SDQ17plus.csv"))
  
  MOOD <<- read.csv(paste0(path,"q_UCL_moodTracker.csv"))
  MOOD$timeStamp <<- stringTimestampToTimestampTZ(timeStamp = MOOD$timeStamp)
  
  BACK2 <<- read.csv(paste0(path,"ucl_2Back.csv"))
  BACK2$timeStamp <<- stringTimestampToTimestampTZ(timeStamp = BACK2$timeStamp)

}

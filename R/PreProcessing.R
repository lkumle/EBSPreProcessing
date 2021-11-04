#' Main Preprocessing function
#' --> calls other sub-functions and runs them in the appropriate order
#' @param path directory with data files
#' @param pathOUT directory that data should be saved to
#' @param return TRUE/FALSE, return data to R environment?
#' @return data --> preprocessed/combined data frame
#' @export

PreProcessing <- function(path, pathOUT, return = T){
  
  # -------------------- # 
  # 1. read in data 
  # -------------------- # 
  dataFrames <- readData(path)
  
  # -------------------- # 
  # 2. create subject database
  # -------------------- # 
  subDatabase <- createDatabase(dataFrames$q_UCL_introQ)
  
  # -------------------- # 
  # 3:combine moodtracker with subject INTRO data
  # -------------------- # 
  print("---------------------------------------")
  data <- addMoodtracker(dataFrames$q_UCL_moodTracker, subDatabase)
  
  #data <- dataMood
  
  # ------------------------------------ # 
  # 4: add task data and Questionnaires
  # ------------------------------------- # 
  
  print("---------------------------------------")
  print("Matching task and questionnaire data to moodtracker data")
  
 
  taskIDs <- gsub(".csv", "", list.files(path, pattern = "ucl_")) # names of task data
  QIDs <- gsub(".csv", "", list.files(path, pattern = "q_"))[-c(1,2)] # names of questinnaire data
  # --> we cut out element 1 and 2 -->intro & moodtracker file is alread added
  
  dataIDs <- c(taskIDs, QIDs) # add all together
  
  for(df in dataIDs){
    
    # get task ID: check if current data set is a task or a questionnaire
    # --> task data sets start with "ucl_", questionnaire data sets start with "q_UCL_"
    # --> then cut off "ucl_" or "q_UCL_" to get name of task
    if(grepl("ucl_", df, fixed = TRUE)){
      ID <- gsub(df, pattern = "ucl_", replacement = "")  #name of task without "ucl_"
      isQ <- F # to notify addTaskData that we will add a task
    } else{
      ID <- gsub(df, pattern = "q_UCL_", replacement = "")  #name of task without "q_UCL_"
      isQ <- T # to notify addTaskData that we will add a questionnaire
    }

    # add data to main data frame
    data <- addTaskData(data, subDatabase, taskID = ID, taskData = dataFrames[[df]], Q = isQ) 
  }

  # --------------------------- # 
  # 5: add aggregated variables
  # --------------------------- #
  
  print("---------------------------------------")
  print("Adding summary statistics")
  data <- addSummaryVariables(data)
  
  
  # --------------------------- # 
  # 7: save files
  # --------------------------- #
  # save files 
  write.csv(data, file = paste0(pathOUT, "AppData_preprocessed.csv"))
  save(data, file = paste0(pathOUT, "AppData_preprocessed.Rdata"))
  
  if(return == T){return(data)}
  
} # end main
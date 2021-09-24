# ---------------------------------------------------------------------- #
#                       APP - DATA EXTRACTION MAIN FILE                  # 
# ---------------------------------------------------------------------- #

rm(list= ls())
# 0. prepare
library(dplyr)

library(devtools) # ADD : download devtools package if required

# ---- set up environment ----- #  
devtools::build() 
devtools::install()
library(PreProcessing)
# ----------------------------- # 

load_all() # load preprocessing functions

# this is where the data is
path = "~/Dropbox/MoodTracker/Data/csv/"


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
data <- addMoodtracker(dataFrames$q_UCL_moodTracker, subDatabase)

# -------------------- # 
# 4: add task data 
# -------------------- # 
# -> get names of all tasks (cut off the .csv to find it in dataFrames)
taskIDs <- gsub(".csv", "", list.files(path, pattern = "ucl_"))

for(task in taskIDs){
  
  #name of task without "ucl_"
  ID <- gsub(task, pattern = "ucl_", replacement = "") 
  
  # add Task data to main data frame
  data <- addTaskData(data, subDatabase, taskID = ID, taskData = dataFrames[[task]]) 
}

# --------------------------- # 
# 5: add questionnaire data 
# --------------------------- # 


# --------------------------- # 
# 6: add aggregated variables
# --------------------------- #



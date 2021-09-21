# ---------------------------------------------------------------------- #
#                       APP - DATA EXTRACTION MAIN FILE                  # 
# ---------------------------------------------------------------------- #


# 0. prepare
library(dplyr)

load_all() # load preprocessing functions

path = "~/Dropbox/MoodTracker/Data/csv/"


# 1. read in data 
readData(path)


# 2. create subject database
subDatabase <- createDatabase(INTRO)


data <- addMoodtracker(MOOD, subDatabase)

data <- add2Back(data,BACK2, subDatabase)

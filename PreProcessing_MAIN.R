# 0. prepare
library(dplyr)

path = "~/Dropbox/MoodTracker/Data/csv/"

# 1. read in data 
readData(path)


# 2. create subject database
subDatabase <- createDatabase(INTRO)




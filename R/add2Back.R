
# ------------------------------------------ # 
# 1: go through all etries in mood data file
# ------------------------------------------ #
for(tp in 1:nrow(BACK2)){
  
  # ------------------------------- # 
  # 2: match with subject DATA
  # ------------------------------- #
  
  subINFO <- BACK2[tp, c("study_id", "deviceModel", "deviceUUID")]
  
  # B: match with entry in subDatabase and retrieve subject data
  subID <- matchSubID(subINFO, subDatabase)
  
  if(is.na(subID)){ # if no entry in subject Database: subset NA ro
    subID <- "unidentifiable"}
  
  # ------------------------------- # 
  # 3: match with timeStamp data
  # ------------------------------- #
  
  # next: 
  # subset data set with all entries of subejct
  
  # compare timestamps
  
  # determine session
  
  # add data 
}



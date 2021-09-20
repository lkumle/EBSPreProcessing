## Task Name = q_UCL_moodTracker,ucl_featureMatchShapes,ucl_featureMatchWords, ....
## Task ID = T1,T2,T3,MT
## Session/Section = a group of obtained rows which have the same interview ID
## Training Session = a group of obtained rows which is not for Moodtracker Task
## Attempt = a group of sessions/sections which initiated by users before or after each other according to its timestamp, mostly start with Moodtracker section
##
## User List :: the list of users and their sessions. Different type of contained lists are briefly explained below:
## Sorted Session List :: The list of sections sorted by timeStamp
## Deduped Session List :: The list of deduplicated sections sorted by timeStamp
## Export Session List :: Filtered Section List for exporting as a file
## Task Name List :: The list of task names
## Frequency List :: The record of how many time each task has been executed for each attempt
## Attempt/Clustered List :: The list of attempts/the list of sessions clustered by its attempt
## Max Attempt Task List :: The list of the maximum number of attempt for EACH task from ALL users, it is used for grouping and arranging columns to answer Q1
##
## Date Group List :: The list of sessions which is grouped by each date on its timestamp.
## Max Frequency List :: The record of the maximum execution of each task has been executed in each attempt
###############################################################################################################################################################

library(plyr)
library(tcltk)
library(Rcpp)

####To check whether package is installed, if not, install it, in main function
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#function to display the input file
getDataPath <- function(){
  path <- file.choose()
  return(path)
}

# search user function, if not , record data cannot run
searchUserString <- function(user_list,user_id){
   result <- -1
   if(length(user_list) >= 1){
    for(i in 1:length(user_list)){
      if(user_list[[i]]$user_id==user_id){
        result <- i
      }
    }
   }
   return(result)
 }

 # search section based on interviewID
 searchSection <- function(section_list, interviewId){
  result <- -1
  if (length(section_list)>=1){
  for (i in 1:length(section_list)){
   if (section_list[[i]]$interview_id == interviewId){
   result <- i
   }
  }
  }
  return (result)
}

 #search section based on its timestamp
 searchSectionByTimestamp <-function(section_list, timeStamp){
  result <- -1
  if (length(section_list)>=1){
  for (i in 1:length(section_list)){
   if (as.character(section_list[[i]]$timeStamp) == as.character(timeStamp)){
   result <- i
   }
  }
  }
  return (result)
}

#deduplicate section in the list
deduplicateSectionList <- function(section_list){
  result <- list()
  for(i in 1:length(section_list)){
    searchIndex <- searchSectionByTimestamp(result,section_list[[i]]$timeStamp)
    if(searchIndex == -1){
      result <- append(result,list(section_list[[i]]))
    }
  }
  return (result)
}

#Timezone Conversion
timeZoneConversion<-function(timeStamp){
  if(grepl('GMT+0000',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0100',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+1')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-1000',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+10')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-1100',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+11')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-1200',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+12')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0200',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+2')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0300',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+3')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0400',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+4')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0500',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+5')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0600',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+6')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0700',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+7')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0800',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+8')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0900',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT+9')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0100',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-1')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1000',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-10')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1100',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-11')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1200',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-12')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1300',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-13')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1400',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-14')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0200',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-2')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0300',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-3')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0400',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-4')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0500',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-5')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0600',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-6')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0700',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-7')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0800',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-8')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0900',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Etc/GMT-9')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0930',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Pacific/Marquesas')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT-0330',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'America/St_Johns')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1245',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Pacific/Chatham')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+1030',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Australia/Lord_Howe')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0930',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Australia/Darwin')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0845',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Australia/Eucla')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0630',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Asia/Yangon')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0545',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Asia/Kathmandu')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0530',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Asia/Colombo')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0430',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Asia/Kabul')), origin="1970-01-01", tz = 'GMT')
  }else if(grepl('GMT+0330',timeStamp,fixed=TRUE)){result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'Asia/Tehran')), origin="1970-01-01", tz = 'GMT')
  }
  return (result)
}

#Convert string Timestamp to Timestamp with time zone conversion if it needs be
stringTimestampToTimestampTZ <- function(timeStamp,timeZoneConversion = TRUE){
  if(timeZoneConversion == TRUE){
    return (timeZoneConversion(timeStamp))
  }else{
    result = as.POSIXct(as.numeric(as.POSIXct(timeStamp, format="%a %b %d %Y %H:%M:%S", tz = 'GMT')), origin="1970-01-01", tz = 'GMT')
    return (result)
  }
}

#Get epoch date for grouping by date
getEpochDate<-function(TimeStamp){
  return (as.numeric(as.POSIXct(as.Date(TimeStamp,format = "%a %b %d %Y %H:%M:%S"), format="%a %b %d %Y %H:%M:%S")))
}

#get index of date_group_list from timeStamp
searchDateGroup <-function(timeStamp,date_group_list){
  index = - 1;

  if(length(date_group_list)>=1){
    for(i in 1:length(date_group_list)){

      if(getEpochDate(timeStamp) == date_group_list[[i]]$date){
        index = i

        return(index)
      }
    }
  }

  return(index)
}

#grouping Tasks by Date
groupTasksByDate <-function(section_list){
  result <- list()
  for(i in 1:length(section_list)){

    convertedTZSearchTimeStamp <- stringTimestampToTimestampTZ(section_list[[i]]$timeStamp)
    searchRes = searchDateGroup(convertedTZSearchTimeStamp,result)

    if(searchRes == -1){
      convertedTZTimeStamp <- stringTimestampToTimestampTZ(section_list[[i]]$timeStamp)
      section = section_list[[i]]
      result <- append(result,list(list(date = getEpochDate(convertedTZTimeStamp),timeStamp = getTimeStampDate(convertedTZTimeStamp), section_list = list(section))))

    }else{
      convertedTZTimeStamp <- stringTimestampToTimestampTZ(section_list[[i]]$timeStamp)
      section = section_list[[i]]
      result[[searchRes]]$section_list <- append(result[[searchRes]]$section_list,list(section))

    }
  }
  return(result)
}

#gather the total number of training days which last more than 10 mins
getNumTrainingDaysMoreThan10Mins <-function(date_list, oneTrainingSessionPerDay_Only = TRUE){
  dateList_NonOneTrainingPerDay <- list()
  dateList_OneTrainingPerDayOnly <- list()
  for(i in 1:length(date_list)){
    date_list[[i]]$filtered_section <- list()
    for(j in 1:length(date_list[[i]]$section_list)){
      print(paste(date_list[[i]]$timeStamp,' ',length(date_list[[i]]$section_list),' ',date_list[[i]]$section_list[[j]]$duration,sep = ""))
      #if this session last equal to or longer than 10 mins
    #  if (as.numeric(date_list[[i]]$section_list[[j]]$duration) >= 600000) {
        #if that time has only one sessions
        date_list[[i]]$filtered_section <- append(date_list[[i]]$filtered_section, date_list[[i]]$timeStamp)
    #  }
    }
  }

  for(i in 1:length(date_list)){
    print(paste(date_list[[i]]$filtered_section,' length:',length(date_list[[i]]$filtered_section)))
    if(oneTrainingSessionPerDay_Only == TRUE){
      if(length(date_list[[i]]$filtered_section) == 1){
        dateList_OneTrainingPerDayOnly <- append(dateList_OneTrainingPerDayOnly, date_list[[i]]$filtered_section[[1]])
      }
    }else{
      if(length(date_list[[i]]$filtered_section) > 1){
        dateList_NonOneTrainingPerDay <- append(dateList_NonOneTrainingPerDay, date_list[[i]]$filtered_section[[1]])
      }
    }
  }

  if(oneTrainingSessionPerDay_Only == TRUE){
    print(dateList_OneTrainingPerDayOnly)
    return(dateList_OneTrainingPerDayOnly)
  }else{
    print(dateList_NonOneTrainingPerDay)
    return(dateList_NonOneTrainingPerDay)
  }
}

#gather the total number of attempt which only do moodtracker task
getMoodTrackerOnlyAttemptNo <- function(session_list){
  MoodTrackerOnlyAttemptNo = 0;
  start = FALSE;
  taskNo = 0
  for(i in 1:length(session_list)){
    msg <- ''

    #if the current row is not the last one in the list
    if(i != length(session_list)){
      #if the task is AutoGenerated (q_UCL_moodTracker)
      if(session_list[[i]]$task == "AutoGenerated (q_UCL_moodTracker)"){
        #if starting point is not defined
        if(start == FALSE){
          #set the starting point
          start = TRUE
          #start count the number of tasks in that attempt
          taskNo = 1
          msg = paste(session_list[[i]]$task," <-- The first time",sep="")
        }else{
          #if starting point is defined but it only has a Moodtracker task previously
          if(taskNo == 1){
              #consider this one as Moodtrack only attempt
              MoodTrackerOnlyAttemptNo = MoodTrackerOnlyAttemptNo + 1
              msg = paste(session_list[[i]]$task," <-- Restart counting, previous one is moodtracker Only Attempt",sep="")
          }else{
              msg = paste(session_list[[i]]$task," <-- Restart counting",sep="")
          }
          #restart counting the number of tasks in that attempt
          taskNo = 1
        }
      }else{
        #if the task is not AutoGenerated (q_UCL_moodTracker), add the number of tasks in that attempt
        taskNo = taskNo + 1
        msg = paste(session_list[[i]]$task," <-- Keep counting",sep="")
      }
    }else{
      #if the task is AutoGenerated (q_UCL_moodTracker)
      if(session_list[[i]]$task == "AutoGenerated (q_UCL_moodTracker)"){
        #if starting point is not defined
        if(start == FALSE){
          #consider this one as Moodtrack only attempt
          MoodTrackerOnlyAttemptNo = MoodTrackerOnlyAttemptNo + 1
          msg = paste(session_list[[i]]$task," <-- The first time, in the last line, previous one is moodtracker Only Attempt",sep="")
        }else{
          #if starting point is defined but it only has a Moodtracker task previously
          if(taskNo == 1){
              #consider this one as Moodtrack only attempt
              MoodTrackerOnlyAttemptNo = MoodTrackerOnlyAttemptNo + 1
              msg = paste(session_list[[i]]$task," <-- Previous one is moodtracker Only Attempt",sep="")
          }
          #restart counting the number of tasks in that attempt
          taskNo = 0
        }
      }else{
        #if the task is not AutoGenerated (q_UCL_moodTracker), add the number of tasks in that attempt
        taskNo = taskNo + 1
        msg = paste(session_list[[i]]$task," <-- Keep counting",sep="")
      }
    }
    print(paste(msg,'(',taskNo,')','::',MoodTrackerOnlyAttemptNo,sep=''))
  }
  return(MoodTrackerOnlyAttemptNo);
}

#function to export file as CSV
exportCSV <- function(dataFrame,fileName){
  if(Sys.info()['sysname'] == "Darwin"){
    print(paste("Filename: ",fileName))
    outputDir <- paste(choose.folder())
    filePath <- paste(fileName, '.csv', sep = '')
    outputFile <- paste(outputDir, filePath, sep='')
    print(outputFile)
    write.table(dataFrame, file = outputFile, row.names=FALSE, col.names=TRUE, sep=',')
  }else{
    print(paste("Filename: ",fileName))
    outputDir <- paste(tclvalue(tkchooseDirectory()),"/",sep="")
    filePath <- paste(fileName, '.csv', sep = '')
    outputFile <- paste(outputDir, filePath, sep='')
    print(outputFile)
    write.table(dataFrame, file = outputFile, row.names=FALSE, col.names=TRUE, sep=',')
  }
}

#function to display folder input
choose.folder <- function() {
	system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
			intern = FALSE, ignore.stderr = TRUE)
	p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
	return(ifelse(length(p), p, NA))
}

#function to append answer of Q2 to Q7 into an export file, detail of each question is in this function
exportQ2_to_7 <- function(user_list,asAFile=FALSE){
   data_export <- data.frame()
   for(i in 1:length(user_list)){

     ####For Q3: Number of training days that include only 1 session (over 10mins)
     tmp_total_number_of_days_1_session_only = length(getNumTrainingDaysMoreThan10Mins(user_list[[i]]$date_list, TRUE))
     tmp_date_list_1_session_only = paste(getNumTrainingDaysMoreThan10Mins(user_list[[i]]$date_list, TRUE),collapse=';')

     ####For Q4:Number of training days that include 2+ sessions (over 10 mins)
     tmp_total_number_of_days = length(getNumTrainingDaysMoreThan10Mins(user_list[[i]]$date_list, FALSE))
     tmp_date_list = paste(getNumTrainingDaysMoreThan10Mins(user_list[[i]]$date_list, FALSE),collapse=';')

     ####For Q6: 6.1) Duration of each session under 10 mins 6.2) Average of under 10 mins session
     sum <- 0
     if (length(user_list[[i]]$uncomplete_session) >= 1){
       user_id = as.character(user_list[[i]]$user_id)
       duration_list = ''
       for(j in 1:length(user_list[[i]]$uncomplete_session)){
         sum <- sum + as.numeric(user_list[[i]]$uncomplete_session[[j]]$duration)
         if(j == 1){
           duration_list <- as.character(user_list[[i]]$uncomplete_session[[j]]$duration)
         }else{
           duration_list <- paste(duration_list, as.character(user_list[[i]]$uncomplete_session[[j]]$duration), sep = ';')
         }
       }
       user_list[[i]]$avg_uncomplete_session <- sum/length(user_list[[i]]$uncomplete_session)
       tmp_data <- data.frame(user_id = user_list[[i]]$user_id, avg_uncomplateSession= user_list[[i]]$avg_uncomplete_session)
     }else{
       user_id = as.character(user_list[[i]]$user_id)
       duration_list = ''
       user_list[[i]]$avg_uncomplete_session <- 0
     }

     #Create number for each row
     #Append each row
     tmp_data <- data.frame(user_id = user_list[[i]]$user_id,
       #Q2&5&7: 2) Number of total training sessions (over 10mins), 5) Number of training sessions (one of the training tasks was open – not just mood tracker) under 10 minutes , 7) Number of mood track only sessions (no training task opened)
       no_of_sessions_under_10_mins = length(user_list[[i]]$uncomplete_sessions), no_of_sessions_over_10_mins = length(user_list[[i]]$complete_sessions) , no_of_moodtracker_only_sessions = getMoodTrackerOnlyAttemptNo(user_list[[i]]$deduped_sorted_sessions),
       #Q3: Number of training days that include only 1 session (over 10mins)
       total_number_of_days_1_session_only =  tmp_total_number_of_days_1_session_only , date_list_1_session_only = tmp_date_list_1_session_only,
       #Q4:Number of training days that include 2+ sessions (over 10 mins)
       total_number_of_days_2plus_sessions =  tmp_total_number_of_days , date_list_2plus_sessions = tmp_date_list,
       #Q6: 6.1) Duration of each session under 10 mins 6.2) Average of under 10 mins session
       avg_uncomplateSession= user_list[[i]]$avg_uncomplete_session ,user_uncomplete_session_duration_list = duration_list)
     data_export <- rbind(data_export,tmp_data)
   }
   if(asAFile == FALSE){
     #if asAFile == FALSE, show data
     return(data_export)
   }else{
     #if asAFile == TRUE, open export the result as CSV file
     exportCSV(data_export,'exportQ2_to_7_file')
   }
 }

#function to append answer of Q1 (i.e. relevant columns of each session)


#function to merge appended Q1 and Q2 to Q7 together, then display or export them as a CSV file


#function to arrange a section according to its timeStamp
arr <- function(section,result_list){


  a <- as.numeric(as.POSIXct(section$timeStamp, format="%a %b %d %Y %H:%M:%S" ))

  if(length(result_list$list) ==0){  #when there is no element yet, add the element and make min and max
    result_list$list <- append (result_list$list,list(section))
    result_list$max<-a
    result_list$min<-a
  }else{ #when there is already element in the list
  if (a<= result_list$min){ # If it is less than min, search for min value position append after 0
    result_list$list <- append (result_list$list,list(section),after=0)
    result_list$min<-a
  } else if (a>=result_list$max){ #If it is more than max, append
    result_list$list <- append (result_list$list,list(section))
    result_list$max<-a
  } else{ #If it is inbetween, start search from 1st element (for loop)
    for (i in 1:length(result_list$list)){

      if (a>as.numeric(as.POSIXct(result_list$list[[i]]$timeStamp, format="%a %b %d %Y %H:%M:%S" )) ){ #if the target is more than list[[i]], keep searching
      }else{ #if the target is less than list[[i]], append i-1
            result_list$list <- append(result_list$list,list(section),after = i-1)
            break #try to break the for loop
      }
    }
  }

  }
  return (result_list)

}

#function to create answer from Q2 to Q7
prepareQ2toQ7AnswerLists <- function(user_list){

  for(i in 1:length(user_list)){

    list <- list()
    min = 0
    max = 0
    result_list <- NULL
    result_list$list<-list
    result_list$min<-min
    result_list$max<-max

      for(j in 1:length(user_list[[i]]$sections)){ # all sections in user

        a<-as.numeric(as.POSIXct(user_list[[i]]$sections[[j]]$timeStamp, format="%a %b %d %Y %H:%M:%S" )) # assign each time stamp for each section user1  to be a to be compared

        result_list <-arr(user_list[[i]]$sections[[j]],result_list) #Example
      }

      user_list[[i]]$sorted_sesstions <- result_list$list
      user_list[[i]]$deduped_sorted_sessions <- deduplicateSectionList(user_list[[i]]$sorted_sesstions)
      user_list[[i]]$date_list <- groupTasksByDate(user_list[[i]]$sorted_sesstions)
   }

   #Number of training sessions
   for(i in 1:length(user_list)){
     uncomplete_session<- list()
     complete_session<- list()
     for(j in 1:length(user_list[[i]]$deduped_sorted_sessions)){ # all deduped_sorted_sessions
       print(user_list[[i]]$user_id)
       print(user_list[[i]]$deduped_sorted_sessions[[j]]$task)
       print(user_list[[i]]$deduped_sorted_sessions[[j]]$duration)
       print(user_list[[i]]$deduped_sorted_sessions[[j]]$duration)
       if(user_list[[i]]$deduped_sorted_sessions[[j]]$task != "Username Questionnaire (C3NL)" && user_list[[i]]$deduped_sorted_sessions[[j]]$task != "AutoGenerated (q_i4i_metaCog)" && user_list[[i]]$deduped_sorted_sessions[[j]]$task != "AutoGenerated (q_i4i_PMH)"){
         print("correct task")
       if (user_list[[i]]$deduped_sorted_sessions[[j]]$task != "AutoGenerated (q_UCL_moodTracker)" && as.numeric(user_list[[i]]$deduped_sorted_sessions[[j]]$duration) < 600000) {
       uncomplete_session <- append (uncomplete_session,list(user_list[[i]]$deduped_sorted_sessions[[j]]))
       print("umcomlete task!")

     } else if (user_list[[i]]$deduped_sorted_sessions[[j]]$task != "AutoGenerated (q_UCL_moodTracker)" && as.numeric(user_list[[i]]$deduped_sorted_sessions[[j]]$duration) >= 600000) {
       complete_session <- append (complete_session,list(user_list[[i]]$deduped_sorted_sessions[[j]]))

       print("complete task!")
       }
     }
     }
     user_list[[i]]$uncomplete_sessions <- uncomplete_session
     user_list[[i]]$complete_sessions <- complete_session
   }

   return(user_list)
}



#function to search tasks group list by task name
searchTasksGroupListByTaskName <- function(keyword,grouped_tasks){
  index = -1
  if(length(grouped_tasks)>=1){
    for(i in 1:length(grouped_tasks)){
      if(grouped_tasks[[i]]$task == keyword){
        index = i
        return(index)
      }
    }
  }
  return(index)
}

#function to group task by task name to create tasks group list for answering Q1
groupTasksByTaskName <- function(deduped_sorted_sessions){
  result <- list()
  for(i in 1:length(deduped_sorted_sessions)){
    searchRes = searchTasksGroupListByTaskName(deduped_sorted_sessions[[i]]$task,result)
    task_id <- ''
    print(paste(i,deduped_sorted_sessions[[i]]$task))
    #population App: changed all task names to be new taks name
    if(deduped_sorted_sessions[[i]]$task == 'AutoGenerated (ucl_2Back)'){
      task_id = 'T1'
    }else if(deduped_sorted_sessions[[i]]$task == 'Backwards Digit Span (UCL)'){
      task_id = 'T2'
    }else if(deduped_sorted_sessions[[i]]$task == 'Emotional Stroop (UCL)'){
      task_id = 'T3'
    }else if(deduped_sorted_sessions[[i]]$task == 'N-Back (UCL)'){
      task_id = 'T4'
    }else if(deduped_sorted_sessions[[i]]$task == 'Affective Set Shifting (UCL)'){
      task_id = 'T5'
    }else if(deduped_sorted_sessions[[i]]$task == 'AutoGenerated (q_UCL_moodTracker)'){
      task_id = 'MT'
    }else{
      task_id = 'NA'
    }
    print(searchRes)
    section = deduped_sorted_sessions[[i]]
    if(task_id != 'NA'){
      if(searchRes == -1){
        result <- append(result,list(list(task = deduped_sorted_sessions[[i]]$task, task_ID = task_id , section_list = list(section))))
        print(paste(i,' ',task_id,' Creating... ','section_list length: ',length(section),sep=''))
      }else{
        result[[searchRes]]$section_list <- append(result[[searchRes]]$section_list,list(section))
        print(paste(i,' ',task_id,' Appending... ','section_list length: ',length(section),' In total of: ',length(result[[searchRes]]$section_list),sep=''))
      }
    }
  }
  return(result)
}

#function to get the maximum number of task, OUTDATED due to "getMaximumNoOfSortedTask"
getMaximumNoOfTask <-function(user_list){
  taskNameList <- getTaskNameList(user_list)
  result <- list()
  for(i in 1:length(taskNameList)){
    maxNoOfTask = 0
    for(k in 1:length(user_list)){

      #Make sure that user list have the section list available
      searchRes = getIndexOfExportSessByTaskName(taskNameList[[i]],user_list[[k]]$export_sessions)
      if(searchRes > 0){
        target = length(user_list[[k]]$export_sessions[[searchRes]]$section_list)
        if(target > maxNoOfTask){
          maxNoOfTask = target
        }
      }
    }
    result <- append(result,maxNoOfTask)
  }
  return(result)
}

#function to get the list of task name
getTaskNameList  <-function(user_list){
  result <- list()
  for(i in 1:length(user_list)){
    for(j in 1:length(user_list[[i]]$export_sessions)){
        for(k in 1:length(user_list[[i]]$export_sessions[[j]]$section_list)){
          if(searchTaskNameInList(result,user_list[[i]]$export_sessions[[j]]$section_list[[1]]$task) == -1){
            result <- append(result,user_list[[i]]$export_sessions[[j]]$section_list[[1]]$task)
          }
        }
    }
  }
  return(result)
}

#search task name in the List
searchTaskNameInList  <-function(taskNameList, taskName){

  index = -1
  if(length(taskNameList) > 0){
    #print('Triggered')
    for(i in 1:length(taskNameList)){
      #print(paste(taskName,taskNameList[[i]],grepl(taskName,as.character(taskNameList[[i]]))))
      if(taskName == taskNameList[[i]]){
        index = i
        return(index)
      }
    }
  }
  return(index)
}


#get index of export session list
getIndexOfExportSessByTaskName <- function(keyword,export_sess){
  index = -1
  if(length(export_sess)>=1){
    for(i in 1:length(export_sess)){
      if(export_sess[[i]]$section_list[[1]]$task == keyword){
        index = i
        return(index)
      }
    }
  }
  return(index)
}

#arrange task name list according to a defined taskOrderList
sortTaskNameList <- function(taskNameList){
  # Population app: changed all tasks names to be new task and left the first one to be moodtracker
  # population app: delete all the (UCL) to see if it fix the error - in testing
  #27/10: Unify taskOrderList
  taskOrderList <- getTaskOrderList()
  result <- list()

  for(i in 1:length(taskOrderList)){
    for(j in 1:length(taskNameList)){
      if(grepl(taskOrderList[[i]],taskNameList[[j]],fixed=TRUE)){
        #print('Triggered')
        result <- append(result,taskNameList[[j]])
      }
    }
  }
  return(result)
}

#obtain the maximum number of sorted task for all users, it is for defining and ordering an answer for Q1
getMaximumNoOfSortedTask <-function(user_list){
  taskNameList <- getTaskNameList(user_list)
  taskNameList <- sortTaskNameList(taskNameList)
  result <- list()
  for(i in 1:length(taskNameList)){
    maxNoOfTask = 0
    for(k in 1:length(user_list)){
      #print(user_list[[k]]$user_id)
      #Make sure that user list have the section list available
      searchRes = getIndexOfExportSessByTaskName(taskNameList[[i]],user_list[[k]]$export_sessions)
      if(searchRes > 0){
        target = length(user_list[[k]]$export_sessions[[searchRes]]$section_list)
        if(target > maxNoOfTask){
          maxNoOfTask = target
        }
      }
    }
    result <- append(result,maxNoOfTask)
  }
  return(result)
}

#task Name to Task ID conversion
taskNameToTaskID <- function(taskName){
  #population app: changed all the taks names to be new tasks
  task_id <- ''
  if(taskName == 'AutoGenerated (ucl_2Back)'){
    task_id = 'T1'
  }else if(taskName == 'Backwards Digit Span (UCL)'){
    task_id = 'T2'
  }else if(taskName == 'Emotional Stroop (UCL)'){
    task_id = 'T3'
  }else if(taskName == 'N-Back (UCL)'){
    task_id = 'T4'
  }else if(taskName == 'Affective Set Shifting (UCL)'){
    task_id = 'T5'
  }else if(taskName == 'AutoGenerated (q_UCL_moodTracker)'){
    task_id = 'MT'
  }else{
    task_id = 'NA'
  }
  return(task_id)
}

#function to search Task name in Frequency List
searchTaskNameInFrequencyList <-function(taskNameList, taskName){
  #print(taskNameList)
  index = -1
  if(length(taskNameList) > 0){
    #print('Triggered')
    for(i in 1:length(taskNameList)){
      if(taskName == taskNameList[[i]]$taskName){
        index = i
        return(index)
      }
    }
  }
  return(index)
}

#function to obtain frequency list from attempt number
getFrequencyListByAttemptNo <- function(attemptNo,clustered_list){
  result <- list()
  for(i in 1:length(clustered_list[[attemptNo]]$sessions)){

    searchIndex = searchTaskNameInFrequencyList(result,clustered_list[[attemptNo]]$sessions[[i]]$taskName)
    if(searchIndex == -1){

      result <- append(result,list(list(taskName = clustered_list[[attemptNo]]$sessions[[i]]$taskName , frequency = 1)))
    }else{

      result[[searchIndex]]$frequency = result[[searchIndex]]$frequency + 1
    }
  }
  return(result)
}

#function to gather attempt list from all users
getAllUserAttemptList <- function(user_list){
  working_user_list <- user_list
  for(i in 1:length(working_user_list)){
    working_user_list[[i]]$attemptList <- getAttemptList(working_user_list[[i]])
  }
  return(working_user_list)
}

#function to gather the maximum number of attempt list from all users, it is used for indexing "getMaxAttemptFrequencyList" function
getMaxAttemptList <- function(user_list){
  maxAttemptList <- list()
  for(i in 1:length(user_list)){
    for(j in 1:length(user_list[[i]]$attemptList)){
      print(paste("User ",i," has ",length(user_list[[i]]$attemptList)," attempt(s)",sep=""))
      print(paste("and in ",j," attempt, it has ",length(user_list[[i]]$attemptList[[j]]$sessions)," session(s)",sep=""))
      if(length(maxAttemptList) < j){
        maxAttemptList[[j]] = length(user_list[[i]]$attemptList[[j]]$sessions)
      }else{
        if(maxAttemptList[[j]] < length(user_list[[i]]$attemptList[[j]]$sessions)){
          maxAttemptList[[j]] = length(user_list[[i]]$attemptList[[j]]$sessions)
        }
      }
    }
  }
  return(maxAttemptList)
}

#function to gather the maximum number of executed attempt from all users, it is used for creating an answer for Q1
getMaxAttemptFrequencyList <- function(user_list){
  maxAttemptList <- getMaxAttemptList(user_list)
  maxFrequencyList <- vector("list", length = length(maxAttemptList))
  for(i in 1:length(maxAttemptList)){
    for(j in 1:length(user_list)){
      if(i <= length(user_list[[j]]$attemptList)){

        for(l in 1:length(user_list[[j]]$attemptList[[i]]$sessions)){

          searchIndex = searchMaxFrequencyListByTaskNameAndAttempt(user_list[[j]]$attemptList[[i]]$sessions[[l]]$taskName,i,maxFrequencyList)

          if(searchIndex == -1){
            maxFrequencyList[[i]] = append(maxFrequencyList[[i]], list(list(taskName = user_list[[j]]$attemptList[[i]]$sessions[[l]]$taskName, frequency = 1)))
          }else{

            maxFrequencyList[[i]][[searchIndex]]$frequency = maxFrequencyList[[i]][[searchIndex]]$frequency + 1
          }
        }
      }
    }
  }
  return(maxFrequencyList)
}

#function to merge appended Q1 and Q2 to Q7 together, then display or export them as a CSV file, , this function is created to support counting each task by the number of attempt as the answer to Q1
exportQ1_to_Q7 <- function(user_list,asAFile=FALSE){
  df1 <- exportQ1(user_list)
  df2 <- exportQ2_to_7(user_list)
  result <- merge(df2,df1, by = "user_id")
  if(asAFile == FALSE){
    #if asAFile == FALSE, show data
    return(result)
  }else{
    #if asAFile == TRUE, open export the result as CSV file
    exportCSV(result,paste('exportQ1_to_Q7_file','_',format(Sys.time(), "%m-%d-%Y_%H-%M-%S"),sep=''))
  }
}

#function to search the clustered list by attempt number and task name, it is used when creating the clustered list to determine whether to create a new attempt or session index
searchClusteredListByAttemptNoAndTaskName <- function(attemptNo,taskName,clustered_list){
  index = list(attemptIndex = -1, taskIndex = -1)
  for(i in 1:length(clustered_list)){
    if(clustered_list[[i]]$attemptNo == attemptNo){
      index$attemptIndex = i
      for(j in 1:length(clustered_list[[i]]$sessions)){
        if(clustered_list[[i]]$sessions[[j]]$taskName == taskName){
          index$taskIndex = j
        }
      }
    }
  }
  return(index)
}

#function to create the clustered/attempt list
getAttemptList <- function(user_list){
  session_list = user_list$deduped_sorted_sessions
  clustered_list = list()
  options(scipen=999)
  print("aaa")
  attemptNo = 0
  startMarker = FALSE
  tmpStartTime = 0
  for(i in 1:length(session_list)){
    print("aaa")
    taskName = session_list[[i]]$task
    if(session_list[[i]]$task == "AutoGenerated (q_UCL_moodTracker)"){
      if(startMarker == FALSE){
        startMarker = TRUE
        tmpStartTime = as.numeric(session_list[[i]]$startTime)
        attemptNo = attemptNo + 1
        algoMsg = "The first time"
      }else{ #When meet another MoodTracker
        tmpStartTime = as.numeric(session_list[[i]]$startTime)
        attemptNo = attemptNo + 1
        algoMsg = "Another moodtracker"
      }
    }else{
      if(as.numeric(session_list[[i]]$startTime) > (tmpStartTime+86400000)){
        tmpStartTime = as.numeric(session_list[[i]]$startTime)
        attemptNo = attemptNo + 1
        algoMsg = paste("Got non-moodtracker task, but this task start time (",session_list[[i]]$startTime,") lasts longer than 24 hrs from the lastest Moodtracker(",tmpStartTime+86400000,")",sep="")
      }else{
        algoMsg = "Got non-moodtracker task, keep counting"
      }
    }
    print(paste(i,"::",taskName,"::",algoMsg,'::','Attempt No.:',attemptNo,sep=''))

    taskInfo = list(taskName = taskName,taskID = taskNameToTaskID(taskName))
    sessionInfo = list(session_list[[i]])
    context = append(sessionInfo,taskInfo)

    if(length(clustered_list) != 0){
      print("Enter A")
      searchIndex = searchClusteredListByAttemptNoAndTaskName(attemptNo,taskName,clustered_list)
      if(searchIndex$attemptIndex != -1){
        attemptIndex = searchIndex$attemptIndex
      #  if(searchIndex$taskIndex  == -1){
          clustered_list[[attemptIndex]]$sessions = append(clustered_list[[attemptIndex]]$sessions, list(context))
      #  }
        #}
      }else{
        clustered_list = append(clustered_list, list(list(attemptNo = attemptNo, sessions = list(context))))
      }

    }else{
      print("Enter B")
      clustered_list = append(clustered_list, list(list(attemptNo = attemptNo, sessions = list(context) )))
    }

  }
  return(clustered_list);
}

#a function to search the list of the maximum number of attempt list by a taskname and an attempt number, it is used for creating the maximum attempt task list
searchMaxAttempTaskListByTaskNameAndAttempt <- function(keyword,attemptNo,list){
  #print("Enter A2")
  if(length(list) > 0){
    for(i in 1:length(list)){
      if(list[[i]][[1]] == keyword){

        for(j in 1:length(list[[i]]$attemptList)){
          if(list[[i]]$attemptList[[j]] == attemptNo){
            return(list(i,j))
          }
        }
        return(list(i,-1))
      }
    }
  }
  return(list(-1,-1))

}

#a function to search the list of the maximum number of attempt list by a taskname, it is used for creating the maximum attempt task list
searchMaxFrequencyListByTaskNameAndAttempt <- function(keyword,attempt,list){

  index = -1
  if(length(list[[attempt]]) != 0){
    for(i in 1:length(list[[attempt]])){
      if(i <= length(list[[attempt]])){
        if(list[[attempt]][[i]]$taskName == keyword){
          return(i)
        }
      }
    }
  }

  return(index)
}

#Get time Stamp date for grouping by date
getTimeStampDate<-function(TimeStamp){
  return (format(as.POSIXct(as.numeric(as.POSIXct(as.Date(TimeStamp,format = "%a %b %d %Y"), format="%a %b %d %Y")), origin="1970-01-01"),"%d/%m/%Y"))
}

#a function to append answer of Q1 (i.e. relevant columns of each session), this function is created to support counting each task by the number of attempt as the answer to Q1
exportQ1 <- function(user_list,asAFile=FALSE){
  data_export <- precreatedColumns(user_list)
  MaxAttFeqList <- getMaxAttemptFrequencyList(user_list)

  #for(i in 305:305){ #For debugging
  for(i in 1:length(user_list)){
    tmp_row <- data.frame()
    for(j in 1:length(user_list[[i]]$export_sessions)){
      tmp_data <- data.frame(user_list[[i]]$user_id,length(user_list[[i]]$attemptList))
      colnames(tmp_data) <- list('user_id','total_attempt')
        for(k in 1:length(user_list[[i]]$export_sessions[[j]]$section_list)){
          if(length(user_list[[i]]$export_sessions[[j]]$section_list[[k]]) >= 4){
            for(l in 5:length(user_list[[i]]$export_sessions[[j]]$section_list[[k]])){
              #27/10: Add taskFeqNo at the end
              col_name = paste(user_list[[i]]$export_sessions[[j]]$section_list[[k]][[l]]$measure,'_',user_list[[i]]$export_sessions[[j]]$section_list[[k]]$task_ID,'_',user_list[[i]]$export_sessions[[j]]$section_list[[k]]$attemptNo,'_',user_list[[i]]$export_sessions[[j]]$section_list[[k]]$taskFeqNo, sep = '')

              tmp_col <- data.frame(user_list[[i]]$export_sessions[[j]]$section_list[[k]][[l]]$value)
              print(paste(col_name,tmp_col,i,j,k,l)) #For debugging
              colnames(tmp_col) <- col_name
              tmp_data <- cbind(tmp_data, tmp_col)

            }
          }
        }
        if(length(user_list[[i]]$export_sessions[[j]]) > 0){
          if(length(tmp_row) == 0){
            tmp_row <- tmp_data
          }else{
            tmp_row <- cbind(tmp_row, tmp_data)
          }
        }
    }
    if(length(data_export) == 0){
      data_export <- tmp_row
    }else{
      data_export <- rbind.fill(data_export, tmp_row)
    }
  }

  if(asAFile == FALSE){
    #if asAFile == FALSE, show data
    return(data_export)
  }else{
    #if asAFile == TRUE, open export the result as CSV file
    exportCSV(data_export,'exportQ1_file')
  }
}

#a function to count a training session
trainingSessionsCounter <- function(user_list){
  for(b in 1:length(user_list)){
    training_session_counter = 1
    non_training_session_counter = 1
    for(i in 1:length(user_list[[b]]$attemptList)){
      training_session_flag = 0
      non_training_session_flag = 0
      print(paste("attempt started",i))
      print(length(user_list[[b]]$attemptList[[i]]$sessions))
      for(j in 1:length(user_list[[b]]$attemptList[[i]]$sessions)){
        print(paste("sessions started",i,j))
        if(length(user_list[[b]]$attemptList[[i]]$sessions) == 1){
          if(user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID == 'MT'){
            user_list[[b]]$attemptList[[i]]$attempt_type = 'non-training session'
            user_list[[b]]$attemptList[[i]]$total_non_training_session_attempt = non_training_session_counter
          }else{
            if(grepl('T1',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
              training_session_flag = 1
              user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
              user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
              #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T1_counter))
              user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
              print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
              #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
            }else if(grepl('T2',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
              training_session_flag = 1
              user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
              user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
              #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T2_counter))
              user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
              print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
              #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
            }else if(grepl('T3',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
              training_session_flag = 1
              user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
              user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
              #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
              user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
              print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
              #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
            }else if(grepl('T4',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
              training_session_flag = 1
              user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
              user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
              #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
              user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
              print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
              #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
            }else if(grepl('T5',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
              training_session_flag = 1
              user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
              user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
              #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
              user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
              print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
              #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
            }
          }
        }else{
          if(grepl('T1',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
            training_session_flag = 1
            user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
            user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
            #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T1_counter))
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
            print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
            #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
          }else if(grepl('T2',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
            training_session_flag = 1
            user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
            user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
            #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T2_counter))
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
            print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
            #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
          }else if(grepl('T3',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
            training_session_flag = 1
            user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
            user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
            #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
            print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
            #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
          }else if(grepl('T4',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
            training_session_flag = 1
            user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
            user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
            #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
            print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
            #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
          }else if(grepl('T5',user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,fixed=TRUE)){
            training_session_flag = 1
            user_list[[b]]$attemptList[[i]]$attempt_type = 'training session'
            user_list[[b]]$attemptList[[i]]$total_training_session_attempt = training_session_counter
            #user_list[[b]]$attemptList[[i]]$sessions[[j]] = append(user_list[[b]]$attemptList[[i]]$sessions[[j]], list(training_session_attempt_no = non_moodtracker_T3_counter))
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
            print(paste("at ",i," attempt(s), non-moodtracker task detected (",user_list[[b]]$attemptList[[i]]$sessions[[j]]$taskID,") , training_session_counter: ",training_session_counter,sep=""))
            #print(user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no)
          }else{
            training_session_flag = 1
            print('moodtracker task detected')
            user_list[[b]]$attemptList[[i]]$sessions[[j]]$training_session_attempt_no = training_session_counter
          }
        }
      }
      if(training_session_flag > 0){
        #print("Triggered")
        training_session_counter = training_session_counter + 1
      }
      if(non_training_session_flag > 0){
        #print("Triggered")
        non_training_session_counter = non_training_session_counter + 1
      }
    }
    print("Ended")
  }
  return(user_list)
}

#a function to get the maximum total training session attempt from all users
getMaxTrainingSessionAttemptNo <- function(user_list){
  result <- list()
  for(b in 1:length(user_list)){
    total_training_session_attempt = 0
    for(i in 1:length(user_list[[b]]$attemptList)){
      # population App : error here !!  Error in if (user_list[[b]]$attemptList[[i]]$attempt_type == "training session") { : argument is of length zero
      if(user_list[[b]]$attemptList[[i]]$attempt_type == "training session"){
        total_training_session_attempt = user_list[[b]]$attemptList[[i]]$total_training_session_attempt
      }
    }
    if(total_training_session_attempt == 0){
      result = append(result,list(0))
    }else{
      result = append(result,total_training_session_attempt)
    }
  }
  return(result)
}

# function to count non-training session
nonTrainingSessionsCounter <- function(user_list){
  maxTrainingSessionAttemptNo <- getMaxTrainingSessionAttemptNo(user_list)
  for(b in 1:length(user_list)){
    non_training_session_counter = maxTrainingSessionAttemptNo[[b]] + 1
    for(i in 1:length(user_list[[b]]$attemptList)){
      for(j in 1:length(user_list[[b]]$attemptList[[i]]$sessions)){
        if(length(user_list[[b]]$attemptList[[i]]$sessions) == 1){
          if(user_list[[b]]$attemptList[[i]]$attempt_type == "non-training session"){
            user_list[[b]]$attemptList[[i]]$sessions[[1]]$training_session_attempt_no = non_training_session_counter
            non_training_session_counter = non_training_session_counter + 1
          }
        }
      }
    }
  }
  return(user_list)
}

#a function to create an answer list for Q1, this function is created to support counting each task by the number of attempt as the answer to Q1
createQ1AnswerList <- function(user_list){

  user_list_clustered <- getAllUserAttemptList(user_list)

  user_list_clustered <- trainingSessionsCounter(user_list_clustered)
  user_list_clustered <- nonTrainingSessionsCounter(user_list_clustered)

  #27/10: Add labelled_user_list
  res <- labelAttTaskFeq(user_list_clustered)
  user_list_clustered <- res$labelled_user_list
  #user_list_clustered$maxFeqList <- res$maxFeqList

  maxAttemptList <- getMaxAttemptTaskList(user_list_clustered)

  for(i in 1:length(user_list_clustered)){
    print(user_list_clustered[[i]]$user_id)
    for(j in 1:length(user_list_clustered[[i]]$attemptList)){
      user_list_clustered[[i]]$export_sessions[[j]] <- list()
      for(k in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions)){

        user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]] <- list()
        user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$task_ID <- user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskID
        user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$task <- user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName

        if(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "AutoGenerated (q_UCL_moodTracker)"){
          print("Enter A")
          #print(paste("AttemptNo: ",user_list_clustered[[i]]$attemptList[[j]]$attemptNo,sep=""))
          #user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$attemptNo <- user_list_clustered[[i]]$attemptList[[j]]$attemptNo
          user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$attemptNo <- user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$training_session_attempt_no
        }else{
          print("Enter B")
          #print(paste("AttemptNo: ",user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$training_session_attempt_no,sep=""))
          user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$attemptNo <- user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$training_session_attempt_no
        }

        #27/10: Define taskFeqNo
        user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$taskFeqNo <- user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskFeqNo

        print(paste(i," ",j," ",k,sep=""))
        if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "AutoGenerated (q_UCL_moodTracker)"){
          print("moodtracker")
          for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
            if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q0.R" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_2"){
              print ("moodtracker RespObject.Q0.R is :")
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q0.R <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
            print (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "score.ImmMoodScores") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$score.ImmMoodScores <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q0.RT" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_3") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q0.RT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q1.R" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_9") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q1.R <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q1.RT" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_10") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q1.RT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q2.R" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_16") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q2.R <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q2.RT" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_17") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q2.RT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q3.R" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "23") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q3.R <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "RespObject.Q3.RT" | user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "_24") {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$RespObject.Q3.RT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "duration" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$duration <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp" ) {
            print("now moodtracker time is :")
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value =user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value )
            print(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }
          }
        } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "AutoGenerated (ucl_2Back)" ) {
            print ("creating T1")
          for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
            if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeNegEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeNegEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeNeuEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeNeuEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimePosEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimePosEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeShapes" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeShapes <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHitsNegEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHitsNegEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHitsNeuEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHitsNeuEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHitsPosEmo" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHitsPosEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHitsShapes" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHitsShapes <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.presOrder" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.presOrder <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp" ) {
            user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
          }
          }
        } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "Backwards Digit Span (UCL)") {
          print ("creating T2")
              for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
              if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.spanEmo"){
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.spanEmo <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                 } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.spanNeu") {
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.spanNeu <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                 } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.span") {
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.span <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                 } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.presOrder") {
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.presOrder <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                 }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp") {
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                 }

              }
           }else if(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "Emotional Stroop (UCL)"){
             print ("creating T3")
             for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
               if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHappyCong"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHappyCong<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)}
               else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHappyIncong"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHappyIncong<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTHappyNeut"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTHappyNeut<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTSadCong"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTSadCong<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTSadIncong"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTSadIncong<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.RTSadNeut"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.RTSadNeut<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accHappyCongRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accHappyCongRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accHappyIncongRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accHappyIncongRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accHappyNeutRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accHappyNeutRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accSadCongRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accSadCongRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accSadIncongRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accSadIncongRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.accSadNeutRaw"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.accSadNeutRaw<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }

             }
           }else if(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "N-Back (UCL)"){
             print ("creating T4")
             for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
               if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeHappy"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeHappy<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)}
               else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeEmo"){
                 user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeEmo<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeNeut"){
                  user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeNeut<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.dPrimeScrambled"){
                   user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.dPrimeScrambled<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
               }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.maxFaceN"){
                    user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.maxFaceN<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.maxN"){
                     user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.maxN<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.maxNeutN"){
                      user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.maxNeutN<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.meanRTCorrHitsHappy"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.meanRTCorrHitsHappy<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.meanRTCorrHitsEmo"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.meanRTCorrHitsEmo<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.meanRTCorrHitsNeut"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.meanRTCorrHitsNeut<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.meanRTCorrHitsScrambled"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.meanRTCorrHitsScrambled<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.presOrder"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.presOrder<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp"){
                       user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp<-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
                }

             }
           }else if(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]]$taskName == "Affective Set Shifting (UCL)"){
              print ("creating T5")
             for (l in 1:length(user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair)){
               if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.emoColNumRandErr" ){
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.emoColNumRandErr <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             } else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.emoColNumRandErrRT" ) {
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.emoColNumRandErrRT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.neutColNumRandErr" ) {
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.neutColNumRandErr <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.neutColNumRandErrRT" ) {
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.neutColNumRandErrRT <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "Scores.presOrder" ) {
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$Scores.presOrder <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             }else if (user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure == "timeStamp" ) {
               user_list_clustered[[i]]$export_sessions[[j]]$section_list[[k]]$timeStamp <-list(measure = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$measure,value = user_list_clustered[[i]]$attemptList[[j]]$sessions[[k]][[1]]$data_pair[[l]]$value)
             }
             }
           }
      }
    }
  }
  return(user_list_clustered)
}

#a function to import a dataset file
import <- function(data,moodtrain = 1){
  user_list <- list()
  for (i in 1:length(data[,1])){
    if(moodtrain == 1){
      #population app: changed task name to be 5 new taks and 1 moodtracker, it seems not working only MT and 2back is working as it contains autogenerated ??
      #popultion app: delete (ucl) in the end of each task to make grepl fucntion working - in testing
      if(grepl("q_UCL_moodTracker",data[i,]$Task) == TRUE | grepl("ucl_2Back",data[i,]$Task) == TRUE | grepl("Backwards Digit Span",data[i,]$Task) == TRUE | grepl("Emotional Stroop",data[i,]$Task) == TRUE | grepl("N-Back",data[i,]$Task) == TRUE | grepl("Affective Set Shifting",data[i,]$Task) == TRUE){
        if (searchUserString(user_list,data[i,]$Username.String)==-1) {
         new_data_pair <- list (measure= as.character(data[i,]$Measure),value = as.character(data[i,]$Value))
         new_section <- list(interview_id = as.character(data[i,]$interview.uuid), data_pair =list(new_data_pair),task= as.character(data[i,]$Task))
         new_user <- list(user_id= as.character(data[i,]$Username.String), sections=list(new_section))
         user_list <- append(user_list, list(new_user))

         # population app: the data strcuture changed timeStamp is the firstline of new user - in testing
         userPosTempNew = length(user_list)
         sectionPosTempNew = length(user_list[[length(user_list)]]$sections)
         if(as.character(data[i,]$Measure == "timeStamp")){
            print("new structure" )
             user_list[[userPosTempNew]]$sections[[sectionPosTempNew]]$timeStamp <- as.character(data[i,]$Value)
             print(paste("user: ",userPosTempNew,sep=""))
             print(paste("section: ",sectionPosTempNew,sep=""))
           }

        }else{
          userPos = searchUserString(user_list,data[i,]$Username.String)
           if(searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))==-1){
             new_data_pair <- list(measure = as.character(data[i,]$Measure),value = as.character(data[i,]$Value))
             new_section <- list(interview_id = as.character(data[i,]$interview.uuid), data_pair =list(new_data_pair),task= as.character(data[i,]$Task))
             user_list[[userPos]]$sections <- append (user_list[[userPos]]$sections, list(new_section))
             #print("new section")
             # population app: dos not need to change here as all the variables are identical in new dataset
              # population app: change tempsectionPos to be 1 because under this condition it is new section - in testing
            #tempsectionPos= searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))
            tempsectionPos = length(user_list[[userPos]]$sections)
            if(as.character(data[i,]$Measure == "timeStamp")){
                print("new structure_2" )
                user_list[[userPos]]$sections[[tempsectionPos]]$timeStamp <- as.character(data[i,]$Value)
                print(paste("user: ",userPos,sep=""))
                print(paste("section: ",tempsectionPos,sep=""))
              }else if(as.character(data[i,]$Measure == "endTime")){
                user_list[[userPos]]$sections[[tempsectionPos]]$endTime <- as.character(data[i,]$Value)
              }else if(as.character(data[i,]$Measure == "startTime")){
                user_list[[userPos]]$sections[[tempsectionPos]]$startTime <- as.character(data[i,]$Value)
              }else if(as.character(data[i,]$Measure == "duration")){
                user_list[[userPos]]$sections[[tempsectionPos]]$duration <- as.character(data[i,]$Value)
              }
           }else{
             sectionPos= searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))
             new_data_pair <- list (measure= as.character(data[i,]$Measure),value = as.character(data[i,]$Value)) ##EDITED CODE
             user_list[[userPos]]$sections[[sectionPos]]$data_pair <- append (user_list[[userPos]]$sections[[sectionPos]]$data_pair, list(new_data_pair))

             # add time stamp to each section assuming time are always record in the middle #
            # population app: dos not need to change here as all the variables are identical in new dataset
             if(as.character(data[i,]$Measure == "timeStamp")){
                 user_list[[userPos]]$sections[[sectionPos]]$timeStamp <- as.character(data[i,]$Value)
                 print(user_list[[userPos]]$sections[[sectionPos]]$timeStamp)
               }else if(as.character(data[i,]$Measure == "endTime")){
                 user_list[[userPos]]$sections[[sectionPos]]$endTime <- as.character(data[i,]$Value)
               }else if(as.character(data[i,]$Measure == "startTime")){
                 user_list[[userPos]]$sections[[sectionPos]]$startTime <- as.character(data[i,]$Value)
               }else if(as.character(data[i,]$Measure == "duration")){
                 user_list[[userPos]]$sections[[sectionPos]]$duration <- as.character(data[i,]$Value)
               }
           }
          }
      }
    }else if(moodtrain == 2){
      if(grepl("q_UCL_moodTracker",data[i,]$Task) == TRUE | grepl("ucl_featureMatchWords",data[i,]$Task) == TRUE | grepl("ucl_featureMatchShapes",data[i,]$Task) == TRUE | grepl("ucl_featureMatchFaces",data[i,]$Task) == TRUE){
        if (searchUserString(user_list,data[i,]$Username.String)==-1) {
         new_data_pair <- list (measure= as.character(data[i,]$Measure),value = as.character(data[i,]$Value))
         new_section <- list(interview_id = as.character(data[i,]$interview.uuid), data_pair =list(new_data_pair),task= as.character(data[i,]$Task))
         new_user <- list(user_id= as.character(data[i,]$Username.String), sections=list(new_section))
         user_list <- append(user_list, list(new_user))
         #print("new user" )

        }else{
         userPos = searchUserString(user_list,data[i,]$Username.String)
           if(searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))==-1){
             new_data_pair <- list(measure = as.character(data[i,]$Measure),value = as.character(data[i,]$Value))
             new_section <- list(interview_id = as.character(data[i,]$interview.uuid), data_pair =list(new_data_pair),task= as.character(data[i,]$Task))
             user_list[[userPos]]$sections <- append (user_list[[userPos]]$sections, list(new_section))
             #print("new section")
            tempsectionPos= searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))
            if(as.character(data[i,]$Measure == "timeStamp")){
                user_list[[userPos]]$sections[[tempsectionPos]]$timeStamp <- as.character(data[i,]$Value)
              }else if(as.character(data[i,]$Measure == "endTime")){
                user_list[[userPos]]$sections[[tempsectionPos]]$endTime <- as.character(data[i,]$Value)
              }else if(as.character(data[i,]$Measure == "startTime")){
                user_list[[userPos]]$sections[[tempsectionPos]]$startTime <- as.character(data[i,]$Value)
              }else if(as.character(data[i,]$Measure == "duration")){
                user_list[[userPos]]$sections[[tempsectionPos]]$duration <- as.character(data[i,]$Value)
              }
           }else{
             sectionPos= searchSection(user_list[[userPos]]$sections,as.character(data[i,]$interview.uuid))
             new_data_pair <- list (measure= as.character(data[i,]$Measure),value = as.character(data[i,]$Value)) ##EDITED CODE
             user_list[[userPos]]$sections[[sectionPos]]$data_pair <- append (user_list[[userPos]]$sections[[sectionPos]]$data_pair, list(new_data_pair))

             # add time stamp to each section assuming time are always record in the middle #
             if(as.character(data[i,]$Measure == "timeStamp")){
                 user_list[[userPos]]$sections[[sectionPos]]$timeStamp <- as.character(data[i,]$Value)
                 print(user_list[[userPos]]$sections[[sectionPos]]$timeStamp)
               }else if(as.character(data[i,]$Measure == "endTime")){
                 user_list[[userPos]]$sections[[sectionPos]]$endTime <- as.character(data[i,]$Value)
               }else if(as.character(data[i,]$Measure == "startTime")){
                 user_list[[userPos]]$sections[[sectionPos]]$startTime <- as.character(data[i,]$Value)
               }else if(as.character(data[i,]$Measure == "duration")){
                 user_list[[userPos]]$sections[[sectionPos]]$duration <- as.character(data[i,]$Value)
               }
           }
          }
      }
    }
  }
  return(user_list)
}

#a function to gather the maximum number of attempt list from all users and arrange coresponding task according to task Order.
getMaxAttemptTaskList <- function(user_list){
  MaxAttempTaskList <- list()
  # Population app: changed all the tasks to be new ones following the order T1_T2_T3_T4_T5_MT
  # population app: possible error cause?? cuz task names has (ucl)-in testing: delete all tasks (ucl), not working still same error
  #27/10: Unify taskOrderList
  taskOrderList <- getTaskOrderList()
  #T1_T2_T3_T1_T2_T3_MT

  #user_list_finalized[[1]]$attemptList[[1]]$sessions[[2]]$taskID

  for(i in 1:length(taskOrderList)){
    for(j in 1:length(user_list)){
      for(k in 1:length(user_list[[j]]$attemptList)){
        for(l in 1:length(user_list[[j]]$attemptList[[k]]$sessions)){
          #print(paste(i,j,k,l))
          if(grepl(taskOrderList[[i]],user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName,fixed=TRUE)){
            # Population app: same as before starts with moodtracker so no need to change here
            if(grepl("q_UCL_moodTracker",user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName,fixed=TRUE)){

              searchIndex = searchMaxAttempTaskListByTaskNameAndAttempt(user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName, user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no, MaxAttempTaskList)
            }else{

              searchIndex = searchMaxAttempTaskListByTaskNameAndAttempt(user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName, user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no, MaxAttempTaskList)
            }
            print(paste("searchIndex: ",searchIndex,sep=""))

            if(searchIndex[[2]] != -1){
            }else{
              if(searchIndex[[1]] == -1){
                print("Enter C1")

                if(grepl("q_UCL_moodTracker",user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName,fixed=TRUE)){

                  MaxAttempTaskList = append(MaxAttempTaskList,list(list(taskName = user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName, attemptList = list(user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no))))
                }else{

                  print(paste(j," ",k," ",l,sep=""))
                  print(user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no)
                  MaxAttempTaskList = append(MaxAttempTaskList,list(list(taskName = user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName, attemptList = list(user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no))))

                }
              }else{
                print("Enter C2")
                if(grepl("q_UCL_moodTracker",user_list[[j]]$attemptList[[k]]$sessions[[l]]$taskName,fixed=TRUE)){

                  MaxAttempTaskList[[searchIndex[[1]]]]$attemptList = append(MaxAttempTaskList[[searchIndex[[1]]]]$attemptList,list(user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no))
                }else{

                  MaxAttempTaskList[[searchIndex[[1]]]]$attemptList = append(MaxAttempTaskList[[searchIndex[[1]]]]$attemptList,list(user_list[[j]]$attemptList[[k]]$sessions[[l]]$training_session_attempt_no))
                }
              }
            }
          }
        }
      }
    }
  }
  for(i in 1:length(MaxAttempTaskList)){
    MaxAttempTaskList[[i]]$attemptList = as.list(unlist(MaxAttempTaskList[[i]]$attemptList)[order(unlist(MaxAttempTaskList[[i]]$attemptList))])
  }
  return(MaxAttempTaskList)
}

#a function to define and order relevant columns for answering Q1, the main difference is this function counting each task by the number of attempt
precreatedColumns <- function(user_list){
  result <- data.frame(character(0),character(0))
  colnames(result) <- list('user_id','total_attempt')
  MaxAttTaskList <- getMaxAttemptTaskList(user_list)
  MaxAttTaskList <- flattenMaxAttemptTaskList(MaxAttTaskList)

  #27/10: get MaxFeqList
  res <- labelAttTaskFeq(user_list)
  MaxFeqList <- res$maxFeqList

  for(i in 1:length(MaxAttTaskList)){
    for(j in 1:length(MaxAttTaskList[[i]]$attemptList)){
      # popolatuion app: add another loop for the most section in in this task - in testing
      #27/10: Change the condition to confrom with MaxFeqList
      #print(paste(i,j))
      tmp_col = list()
      if(MaxAttTaskList[[i]]$taskName == "AutoGenerated (q_UCL_moodTracker)"){
        tmp_feq = MaxFeqList[[6]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating MT")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("RespObject.Q0.R_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("RespObject.Q0.RT_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("RespObject.Q1.R_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("RespObject.Q1.RT_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("RespObject.Q2.R_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
            paste("RespObject.Q2.RT_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
            paste("RespObject.Q3.R_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("RespObject.Q3.RT_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("score.ImmMoodScores","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("duration_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("timeStamp_MT","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }else if(MaxAttTaskList[[i]]$taskName == "AutoGenerated (ucl_2Back)"){
        tmp_feq = MaxFeqList[[1]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating T1")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("Scores.dPrimeNegEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimeNeuEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimePosEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimeShapes_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTHitsNegEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                        paste("Scores.RTHitsNeuEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTHitsPosEmo_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTHitsShapes_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.presOrder_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("timeStamp_T1","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }else if(MaxAttTaskList[[i]]$taskName == "Backwards Digit Span (UCL)"){
        tmp_feq = MaxFeqList[[2]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating T2")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("Scores.spanEmo_T2","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.spanNeu_T2","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.span_T2","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.presOrder_T2","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("timeStamp_T2","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }else if(MaxAttTaskList[[i]]$taskName == "Emotional Stroop (UCL)"){
        tmp_feq = MaxFeqList[[3]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating T3")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("Scores.RTHappyCong_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTHappyIncong_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTHappyNeut_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTSadCong_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.RTSadIncong_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.RTSadNeut_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.accHappyCongRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.accHappyIncongRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.accHappyNeutRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.accSadCongRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.accSadIncongRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k, sep=""),paste("Scores.accSadNeutRaw_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""), paste("timeStamp_T3","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }else if(MaxAttTaskList[[i]]$taskName == "N-Back (UCL)"){
        tmp_feq = MaxFeqList[[4]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating T4")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("Scores.dPrimeHappy_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimeEmo_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimeNeut_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.dPrimeScrambled_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.maxFaceN_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.maxN_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.maxNeutN_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.meanRTCorrHitsHappy_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.meanRTCorrHitsEmo_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.meanRTCorrHitsNeut_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
                                                                                                  paste("Scores.meanRTCorrHitsScrambled_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k, sep=""),paste("Scores.presOrder_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""), paste("timeStamp_T4","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }else if(MaxAttTaskList[[i]]$taskName == "Affective Set Shifting (UCL)"){
        tmp_feq = MaxFeqList[[5]]$Feq[[MaxAttTaskList[[i]]$attemptList[[j]]]]
        #print(paste("Feq: ",tmp_feq))
        if(tmp_feq>0){
          for(k in 1:tmp_feq){
            print ("generating T5")
            tmp_col = data.frame(character(0),character(0),character(0),character(0),character(0),character(0))
            colnames(tmp_col) <- list(paste("Scores.emoColNumRandErr_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.emoColNumRandErrRT_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.neutColNumRandErr_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.neutColNumRandErrRT_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),paste("Scores.presOrder_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""),
            paste("timeStamp_T5","_",MaxAttTaskList[[i]]$attemptList[[j]],"_",k,sep=""))
            result <- cbind(result, tmp_col)
          }
        }
      }
      #result <- cbind(result, tmp_col)
    }
  }
  return(result)
}

#a function to gather the total number of attempts in the list for creating columns
flattenMaxAttemptTaskList <- function(MaxAttTaskList){
  maxTrainingSessionAttemptNo = 0
  for(i in 1:length(MaxAttTaskList)){
    if(grepl("q_UCL_moodTracker",MaxAttTaskList[[i]]$taskName,fixed=TRUE) == FALSE){
      if(MaxAttTaskList[[i]]$attemptList[[length(MaxAttTaskList[[i]]$attemptList)]] >= maxTrainingSessionAttemptNo){
        #maxTrainingSessionAttemptNo = length(MaxAttTaskList[[i]]$attemptList)
        print(MaxAttTaskList[[i]]$attemptList[[length(MaxAttTaskList[[i]]$attemptList)]])
        maxTrainingSessionAttemptNo = MaxAttTaskList[[i]]$attemptList[[length(MaxAttTaskList[[i]]$attemptList)]]
      }
    }
  }
  #print(maxTrainingSessionAttemptNo)

  tempAttemptList = list()
  for(i in 1:maxTrainingSessionAttemptNo){
    tempAttemptList = append(tempAttemptList,i)
  }

  for(i in 1:length(MaxAttTaskList)){
    if(grepl("q_UCL_moodTracker",MaxAttTaskList[[i]]$taskName,fixed=TRUE) == FALSE){
      MaxAttTaskList[[i]]$attemptList = tempAttemptList
    }
  }

  return(MaxAttTaskList)
}

getTaskOrderList <- function(){
  return(list("ucl_2Back","Backwards Digit Span","Emotional Stroop","N-Back","Affective Set Shifting","q_UCL_moodTracker"))
}

#28/10: Revised this function
labelAttTaskFeq <- function(user_list){
  #This function have two outputs, laballed_user_list is user_list which each task is marked based on the ocurrence of that task in that attempt, maxFeqList is the list of maxmium times that each task has been executed in each attempt
  result <- list(labelled_user_list = "" , maxFeqList = "")
  maxAttemptList <- getMaxAttemptList(user_list)

  MaxAttTaskList <- getMaxAttemptTaskList(user_list)
  #MaxAttTaskList <- flattenMaxAttemptTaskList(MaxAttTaskList)

  tmp_user_list <- user_list
  maxFeqList <- vector("list", length = length(MaxAttTaskList)) #Assign maxFeqList in the same length of taskOrderList

  #For each task
  for(i in 1:length(MaxAttTaskList)){
  #for(i in 1:1){ #For debugging
    maxFeqList[[i]]$task <- MaxAttTaskList[[i]]$taskName
    maxFeqList[[i]]$Feq <- rep(list(0),length(maxAttemptList))

    for(j in 1:length(maxAttemptList)){
      maxFeqList[[i]]$Feq[[j]] <- getMaxTaskFeqByAttempt(maxFeqList[[i]]$task,j,tmp_user_list)
    }
  }

  result$labelled_user_list = labelTaskFeqByAttempt(getTaskOrderList(),maxAttemptList,tmp_user_list)
  result$maxFeqList = maxFeqList

  return(result)
}

#For Debugging purpose
searchIndexByTaskFeq <- function(feq,user_list){
  for(i in 1:length(user_list)){
    for(j in 1:length(user_list[[i]]$attempt)){
      for(k in 1:length(user_list[[i]]$attempt[[j]]$sessions)){
        if(user_list[[i]]$attempt[[j]]$sessions[[k]]$taskFeqNo == feq){
          print(paste(i,j,k))
        }
      }
    }
  }
}

#28/10: Create this function
getMaxTaskFeqByAttempt <- function(taskName,attemptNo,user_list){
  result = 0
  for(i in 1:length(user_list)){
    tmp_feq = 0
    for(j in 1:length(user_list[[i]]$attemptList)){
      for(k in 1:length(user_list[[i]]$attemptList[[j]]$sessions)){
        if(user_list[[i]]$attemptList[[j]]$sessions[[k]]$training_session_attempt_no == attemptNo){
          #print(paste(i,j,k))
          #print(paste(user_list[[i]]$attemptList[[j]]$sessions[[k]]$taskName,taskName))
          if(grepl(user_list[[i]]$attemptList[[j]]$sessions[[k]]$taskName,taskName,fixed=TRUE)){
            tmp_feq = tmp_feq + 1
          }
        }
      }
    }
    if(tmp_feq > result){
      result = tmp_feq
    }
  }
  return(result)
}

#28/10: Create this function
labelTaskFeqByAttempt <- function(taskNameList,maxAttemptList,user_list){
  tmp_user_list <- user_list
  for(i in 1:length(taskNameList)){
    for(j in 1:length(maxAttemptList)){
      tmp_max_feq = 0
      #For each user
      for(k in 1:length(tmp_user_list)){
        tmp_feq = 0
        for(l in 1:length(tmp_user_list[[k]]$attemptList)){
          #print(paste(length(tmp_user_list[[k]]$attemptList),j))
          #For each session
          for(m in 1:length(tmp_user_list[[k]]$attemptList[[l]]$sessions)){
            #If taskOrderList[[i]] is found as a task name in tmp_user_list[[k]]$attemptList[[j]]$sessions[[l]]
            if(tmp_user_list[[k]]$attemptList[[l]]$sessions[[m]]$training_session_attempt_no == j){
              #print(paste(tmp_user_list[[k]]$attemptList[[l]]$sessions[[m]]$training_session_attempt_no,j))
              if(grepl(taskNameList[[i]],tmp_user_list[[k]]$attemptList[[l]]$sessions[[m]]$taskName,fixed=TRUE)){
                #print(paste(taskNameList[[i]],tmp_user_list[[k]]$attemptList[[l]]$sessions[[m]]$taskName,taskNameList[[i]]))
                #Increase the frequency
                tmp_feq = tmp_feq + 1
                #Define the frequency in the list
                tmp_user_list[[k]]$attemptList[[l]]$sessions[[m]]$taskFeqNo = tmp_feq
              }
            }
          }
        }
        #If tmp_feq is more than tmp_max_feq
        if(tmp_feq > tmp_max_feq){
          #Assign tmp_max_feq to be tmp_feq
          tmp_max_feq = tmp_feq
        }
      }
    }
  }
  return(tmp_user_list)
}

#################Complied function

main <- function(moodtrain){
  usePackage('Rcpp')
  usePackage('plyr')
  usePackage('tcltk')

  ####Installing prequisites , in case the above function is not working
  #install.packages('Rcpp')
  #install.packages('plyr')

  Import_Path <- getDataPath()
  data <- read.csv(file=Import_Path, header=TRUE, sep=",")

  #user_list_imported <- import(data)
  user_list_imported <- import(data, moodtrain)
  user_list_prepared <- prepareQ2toQ7AnswerLists(user_list_imported)
  #user_list_finalized <- createQ1AnswerList(user_list_prepared)
  #exportQ1_to_Q7(user_list_finalized,TRUE)
  user_list_finalized <- createQ1AnswerList(user_list_prepared)
  exportQ1_to_Q7(user_list_finalized,TRUE)

}

# running data from moodtrain 1
main(moodtrain = 1)

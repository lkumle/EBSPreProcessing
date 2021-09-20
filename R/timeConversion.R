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
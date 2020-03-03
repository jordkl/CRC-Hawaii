library(exifr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(magrittr)
library(RODBC)
library(filesstrings)

#directory to video files
directoryGopro <- "E:\\2020FEBKauai\\GoPro"

#get file list
files <- list.files(directoryGopro, recursive=TRUE, full.names=TRUE)
exifinfo <- read_exif(files)

#Filter
exportGoPro <- exifinfo %>% 
  filter(SerialNumberHash != "b28c2f8da2b9a90f570861decf4b033f") %>% #Filter out Helm JKL (merge back later) check serial number
  subset(select = c(SourceFile, FileName, CreateDate)) %>% 
  mutate(CreateDate = ymd_hms(CreateDate, tz = "HST"))

#Read sightings file
db <- "\\\\CRCdata1\\RedirectedFolders\\jlerma\\Desktop\\2020FEBKauai\\Hawaii sighting database_Feb2020v1.accdb"
con <- odbcConnectAccess2007(db)

sightings <- sqlFetch(con,"Sighting and Permit info", as.is = TRUE) %>%
  mutate(`Start time` = as.POSIXct(`Start time`, format = "%Y-%m-%d %H:%M:%S", tz = "HST")) %>% 
  mutate(`End time` = as.POSIXct(`End time`, format = "%Y-%m-%d %H:%M:%S", tz = "HST")) %>%
  mutate(start.time = strftime(`Start time`, format = "%H:%M:%S", tz = "HST")) %>% 
  mutate(end.time = strftime(`End time`, format = "%H:%M:%S", tz = "HST")) %>%
  mutate(Date = strftime(Date, format = "%Y-%m-%d", tz = "HST")) %>% 
  unite("CreateDate",c(Date,start.time), sep = " ", remove = FALSE) %>% 
  unite("EndDate",c(Date,end.time), sep = " ", remove = FALSE) %>% 
  mutate(CreateDate = as.POSIXct(CreateDate, format = "%Y-%m-%d %H:%M:%S", tz = "HST")) %>% 
  mutate(EndDate = as.POSIXct(EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "HST")) %>% 
  filter(CreateDate > as.POSIXct("2020-01-02", tz = "HST", format = "%Y-%m-%d"))
  
  odbcCloseAll()

#Sort
  for (i in 1:nrow(sightings)){
    if (sightings$Date[i] < 2020) {
      print("Dates out of range, check sighting dates!")
    } else {
vids <- exportGoPro[which(exportGoPro$CreateDate >= sightings$CreateDate[i] & 
                            exportGoPro$CreateDate <= sightings$EndDate[i]),]
dest <- paste("D:/2020FEBKauai/Photos by Species/", sightings$Species[i], "/2020FEB0", 
              day(sightings$Date[i]), "_ENC0", sightings$`Sighting #`[i], sep = "")
if (length(vids) > 0) {
move_files(vids$SourceFile, destinations = dest)
}
}
  }
  
#To Do
  #Check serial number for files being filtered out
  #Check exif file for jpg - creation date?
  #standardize for use in future field projects
  #check move files to make sure it copies instead of cuts
  #make error message more specific date range
  #make options at top of code for generalization: filter by project start date, database location

### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Project
# 3. combine multiple Campaigns into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository


rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
#library(httr)
#library(jsonlite)
#library(R.utils)
# To connect to GitHub
library(RCurl)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"project.example" 

## Folder Structure ----
# This script uses one main folder ('working directory')
# Three subfolders will be created within the 'working directory'. They are 'EM Export','Staging' and 'Tidy data'
# Save database exports into the 'EM Export' folder
# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)

# **The only folder you will need to create is your working directory and EM Export folder**

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
export.dir<-paste(working.dir,"EM Export",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")

setwd(working.dir)

## Create EM Export, Staging and Tidy data folders ----
dir.create(file.path(working.dir, "Staging"))
dir.create(file.path(working.dir, "Tidy data"))

## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")

# Combine all data----
# The below code will find all files that have the same ending (e.g. "_Metadata.csv") and bind them together.
# The end product is three data frames; metadata, maxn and length.

metadata <-list.files.GA("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~read_files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

unique(metadata$project) # check the number of projects in metadata
unique(metadata$campaignid) # check the number of campaigns in metadata

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine MaxN files ----

# Combine all downloaded Point .txt files into one data frame
points <-list.files.GA("_Points.txt")%>% # list all files ending in "_Points.txt"
  purrr::map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>% # Leaving this line on will only keep the 7 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

# Turn points into maxn, combine with the metadata and only keep drops that were successful for count.
maxn<-points%>%
  dplyr::group_by(campaignid,sample,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::filter(!maxn==0)%>%
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.count=="Yes")%>% 
  glimpse()

setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

## Combine Lengths and 3D point files ----

# Combine all downloaded 3D Points .txt files into one data frame
threedpoints.files <-list.files.GA("3DPoints.txt") # list all files ending in "3DPoints.txt"
threedpoints.files$lines<-sapply(threedpoints.files,countLines) # Count lines in files (to avoid empty files breaking the script)

threedpoints<-expand.files(threedpoints.files)%>% # remove all empty files
  purrr::map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,range,number)%>% # Leaving this line on will only keep the 8 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse() 

# Combine all downloaded Lengths txt files into one data frame
length.files <-list.files.GA("Lengths.txt") # list all files ending in "Lengths.txt"
length.files$lines<-sapply(length.files,countLines) # Count lines in files (to avoid empty files breaking the script)

lengths<-expand.files(length.files)%>% # remove all empty files
  purrr::map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,length,range,number)%>% # Leaving this line on will only keep the 9 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

# Combine lengths and 3D points together, combine with metadata and only keep drops that were successful for length.
length3dpoints<-lengths%>%
  plyr::rbind.fill(threedpoints)%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::inner_join(metadata,by=c("project","campaignid","sample"))%>%
  dplyr::filter(successful.length=="Yes")%>%
  glimpse()

setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

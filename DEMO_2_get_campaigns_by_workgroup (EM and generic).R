rm(list=ls()) # Clear memory

## Load Libraries ----
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") # will not re-install if it hasn't been updated since last install
library(GlobalArchive)
library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(purrr)
library(RCurl)
library(readr)
library(R.utils)
library(stringr)
library(tidyr)

## Set Study Name ----
study<-"workgroup.example" # Change this to suit your study name. This will also be the prefix on your final saved files.

## File Structure ----
# This script works out of one main folder (this is called your 'working directory')
# There are three subfolders within the 'working directory'. They are 'Downloads','Data' and 'Tidy data'
# The 'Downloads' folder is used to save the unaltered files downloaded from GlobalArchive.
# The 'Data' folder is used to save the initial outputs from these scripts (e.g. the combined metadata files) NOTE: These initial outputs have not gone through any     validation steps (e.g. any of the checks against the life-history sheet)

# **The only folder you will need to create outside of R is your working directory**

## Set your working directory ----
working.dir<-("C:/GitHub/globalarchive-query") # Paste your working directory here (note the use of forward slash)

setwd(working.dir)

## Save these directory names to use later----
data.dir=paste(working.dir,"Data",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir=paste(working.dir,"Tidy data",sep="/")

## Clear downloads folder ----
# If you have already ran this script and downloaded data from GlobalArchive it is wise to run this next line
# It will delete any data sitting within your 'Downloads' folder 
# DO NOT SAVE ANY OTHER FILES IN YOUR DOWNLOADS FILE
# After running this line they will not be recoverable
# This avoids doubling up files, or including files that are not meant to be included.

unlink(download.dir, recursive=TRUE) 

## Create a folder for downloaded data and tidy data ----
# The below three lines will create the three subfolders within your working directory ('Downloads','Data' and 'Tidy data')
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Data"))
dir.create(file.path(working.dir, "Tidy data"))

# Load some more functions that are used to download data from GlobalArchive ----
source("https://raw.githubusercontent.com/GlobalArchiveManual/globalarchive-query/master/values.R")

## Setup your query ----
# Add your API user token ----
# The first step in setting up your query is to enter your API. This token allows R to communicate with GlobalArchive

# Finding your API
# 1. Go to GlobalArchive and login.
# 2. On the front page Click 'Data.
# 3. In the top right corner click on your name, then 'API token'
# 4. Generate an API token and copy it.
# 5. Paste it below

# Alternatively you can use the demo user API (Saved below), but this will only allow you download data shared with the demo user
API_USER_TOKEN <- "15b4edc7330c2efadff018bcc5fd684fd346fcaef2bf8a7e038e56c3"

# Set up your search criteria ----
# A number of example search criterias are given in the read me on the 'globalarchive-query' github repository.
# See: https://github.com/GlobalArchiveManual/globalarchive-query
# In this example we are searching for a WORKGROUP called "Example: merging different data types"
# NOTE: change spaces in the project name to '+'

q<-query.workgroup("Example:+merging+different+data+types")

## Download data ----
# Run the next line to start downloading the files matching the search above
# These files will be saved in the 'Downloads' folder within your working directory folder
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

# Combine all downloaded data together ----
# Your data is now downloaded into many folders within the 'Downloads' folder.
# The below code will go into each of these folder and find all files that have the same ending and bind them together.
# In this example there are campaigns that have Eventmeasure exports (_Points.txt, _Lengths.txt and 3Dpoints.txt files) and generic files (Count.csv and Length.csv files)
# The end result is three data frames; metadata, maxn and length.

## Metadata files ----
# Combine all downloaded metadata .csv files into one data frame
metadata <-list.files.GA("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
  map_df(~read_files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>% # Leaving this line on will only keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

## Eventmeasure files ----
## Points files ----
# Combine all downloaded Point .txt files into one data frame
points <-list.files.GA("_Points.txt")%>% # list all files ending in "_Points.txt"
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>% # Leaving this line on will only keep the 7 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

## 3D Points files ----
# Combine all downloaded 3D Points .txt files into one data frame
threedpoints.files <-list.files.GA("3DPoints.txt") # list all files ending in "3DPoints.txt"
threedpoints.files$lines<-sapply(threedpoints.files,countLines) # Count lines in files (to avoid empty files breaking the script)

threedpoints<-expand.files(threedpoints.files)%>% # remove all empty files
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,range,number)%>% # Leaving this line on will only keep the 8 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse() 

## Lengths files ----
# Combine all downloaded Lengths txt files into one data frame
length.files <-list.files.GA("Lengths.txt") # list all files ending in "Lengths.txt"
length.files$lines<-sapply(length.files,countLines) # Count lines in files (to avoid empty files breaking the script)

lengths<-expand.files(length.files)%>% # remove all empty files
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,length,range,number)%>% # Leaving this line on will only keep the 9 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

### Generic Campaigns ----
## Count fles ----
# Combine all downloaded generic Count.csv files into one data frame
count <-list.files.GA("Count.csv")%>% # list all files ending in "Count.csv"
  map_df(~read_files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,count)%>% # Leaving this line on will only keep the 7 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

## Length files ----
# Combine all downloaded generic Length.csv files into one data frame
length <-list.files.GA("Length.csv")%>% # list all files ending in "Length.csv"
  map_df(~read_files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,length,count)%>% # Leaving this line on will only keep the 8 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

## Make Maxn and length dataframes----
# Turn points into maxn, combine with the generic count. Join metadata and only keep drops that were successful for count.
maxn<-points%>%
  group_by(campaignid,sample,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  slice(which.max(maxn))%>%
  ungroup()%>%
  filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>% 
  plyr::rbind.fill(count)%>% # Add in generic count data
  inner_join(metadata)%>%
  replace_na(list(maxn=0))%>%
  dplyr::filter(successful.count=="Yes")%>%
  dplyr::mutate(maxn=ifelse(maxn%in%c(0,NA),count,maxn))%>% 
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::select(-c(count))%>%
  filter(maxn>0)%>%
  glimpse()

# Combine lengths, generic lengths and 3D points together. Join metadata and only keep drops that were successful for length.
length3dpoints<-lengths%>%
  plyr::rbind.fill(threedpoints)%>%
  plyr::rbind.fill(length)%>%
  dplyr::mutate(number=ifelse(number%in%c(0,NA),count,number))%>%
  dplyr::select(-c(count))%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  inner_join(metadata)%>%
  filter(successful.length=="Yes")%>%
  glimpse()

## Combine "Campaign Information" ----
# Combine extra information fields saved on GlobalArchive e.g. Sampling Method, Image Analysis Method, etc...

uniq.campaign <- unique(unlist(metadata$campaignid)) # Use metadata to make a list to re-save info with campagn name 

info.join<- data.frame()%>% # create a blank dataframe with campaignid and Project
  c("campaignid","project")%>%
  map_dfr( ~tibble(!!.x := logical() ) )

for (i in 1:length(uniq.campaign)){
  metadata.sub <- subset(metadata, campaignid == uniq.campaign[i]) # Subset metadata to get project info
  project<-as.character(unique(metadata.sub$project)) # save project name
  campaignid<-as.character(unique(metadata.sub$campaignid)) # save campaign name
  setwd(paste(download.dir,project,campaignid,sep="/")) # set wd
  info<-read.csv(".info.csv") # read in info csv 
  df <- data.frame(info)%>% # make a dataframe
    mutate(campaignid=as.character(campaignid))%>% # add in campaign id
    mutate(project=as.character(project)) # add in project
  info.join <- rbind(info.join,df) # add new dataframe to blank dataframe, loop will then add in the next campaign
}

# If this loop breaks it is because the info.csv downloaded is blank
# This means no extra "Campaign Information" has been saved.
# "Campaign Information" will need to be added to the campaign on GlobalArchive and then re-run line 78 (the line beginning with nresults)

# Re format the "Campaign Information" so each CampaignID is a row
info<-info.join%>%
  select(project,campaignid,name,value)%>%
  spread(.,name,value)

unique(metadata$campaignid)%>%sort() # Check the number of campaigns within metadata
unique(info$campaignid) # Check the number of campaigns with extra information

## Save maxn and length files ----
setwd(data.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

# Save metadata and "Campaign Information"
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)
write.csv(info,paste(study,"info.csv",sep="_"),row.names = FALSE)

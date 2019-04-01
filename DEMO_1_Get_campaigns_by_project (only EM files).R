
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
library(httr)
library(jsonlite)
library(R.utils)
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

# Three subfolders will be created within the 'working directory'. They are 'Downloads','Staging' and 'Tidy data'

# The 'Downloads' folder saves files downloaded from GlobalArchive.

# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any     validation steps (e.g. any of the checks against the life-history sheet)

# **The only folder you will need to create outside of R is your working directory**

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own



## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")



## Delete Downloads folder ----
# It will delete any data sitting within your 'Downloads' folder 
# DO NOT SAVE ANY OTHER FILES IN YOUR DOWNLOADS FILE
# After running this line they will not be recoverable
# This avoids doubling up GlobalArchive files, or including files from other Projects.
setwd(working.dir)
unlink(download.dir, recursive=TRUE) 


## Create Downloads, Staging and Tidy data folders ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Staging"))
dir.create(file.path(working.dir, "Tidy data"))



## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/GlobalArchiveManual/globalarchive-query/master/values.R")

# An API token allows R to communicate with GlobalArchive

# Finding your API token
# 1. Go to GlobalArchive and login.
# 2. On the front page Click 'Data'.
# 3. In the top right corner click on your name, then 'API token'
# 4. Generate an API token and copy it.
# 5. Paste it below

# Alternatively you can use the demo user API (15b4edc7330c2efadff018bcc5fd684fd346fcaef2bf8a7e038e56c3)

# Add your personal API user token ----
API_USER_TOKEN <- "15b4edc7330c2efadff018bcc5fd684fd346fcaef2bf8a7e038e56c3"


# Set up your query ----

# A number of example queries are given in the read me on the 'globalarchive-query' github repository.
# See: https://github.com/GlobalArchiveManual/globalarchive-query

# In this example we are searching for a PROJECT called "Pilbara Marine Conservation Partnership"
# NOTE: change any spaces in the project name to '+'

q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Pilbara+Marine+Conservation+Partnership"}}]}'


## Download data ----
# These files will be saved in the 'Downloads' folder within your working directory folder
  
ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)



# Combine all downloaded data----



# Your data is now downloaded into many folders within the 'Downloads' folder. (You can open File Explorer or use the Files Pane to check)
# The below code will go into each of these folders and find all files that have the same ending (e.g. "_Metadata.csv") and bind them together.
# The end product is three data frames; metadata, maxn and length.



## Combine Metadata files ----

metadata <-list.files.GA("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
  map_df(~read_files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)



## Combine MaxN files ----

# Combine all downloaded Point .txt files into one data frame
points <-list.files.GA("_Points.txt")%>% # list all files ending in "_Points.txt"
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>% # Leaving this line on will only keep the 7 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

# Turn points into maxn, combine with the metadata and only keep drops that were successful for count.
maxn<-points%>%
  group_by(campaignid,sample,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  slice(which.max(maxn))%>%
  ungroup()%>%
  filter(!is.na(maxn))%>%
  filter(!maxn==0)%>%
  inner_join(metadata)%>%
  filter(successful.count=="Yes")%>% 
  glimpse()

setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)



## Combine Lengths and 3D point files ----

# Combine all downloaded 3D Points .txt files into one data frame
threedpoints.files <-list.files.GA("3DPoints.txt") # list all files ending in "3DPoints.txt"
threedpoints.files$lines<-sapply(threedpoints.files,countLines) # Count lines in files (to avoid empty files breaking the script)

threedpoints<-expand.files(threedpoints.files)%>% # remove all empty files
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,range,number)%>% # Leaving this line on will only keep the 8 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse() 

# Combine all downloaded Lengths txt files into one data frame
length.files <-list.files.GA("Lengths.txt") # list all files ending in "Lengths.txt"
length.files$lines<-sapply(length.files,countLines) # Count lines in files (to avoid empty files breaking the script)

lengths<-expand.files(length.files)%>% # remove all empty files
  map_df(~read_files_txt(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,family,genus,species,length,range,number)%>% # Leaving this line on will only keep the 9 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

# Combine lengths and 3D points together, combine with metadata and only keep drops that were successful for length.
length3dpoints<-lengths%>%
  plyr::rbind.fill(threedpoints)%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  inner_join(metadata,by=c("project","campaignid","sample"))%>%
  filter(successful.length=="Yes")%>%
  glimpse()

setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

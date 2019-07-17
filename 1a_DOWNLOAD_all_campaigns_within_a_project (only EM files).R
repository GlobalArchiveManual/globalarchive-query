
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
# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)
# **The only folder you will need to create is your working directory**

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
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")


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

## Download data ----
# These files will be saved in the 'Downloads' folder within your working directory folder
# In this example we are searching for a PROJECT called "Pilbara Marine Conservation Partnership"
# NOTE: change any spaces in the project name to '+'

ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, 
                     q=ga.query.project("Pilbara+Marine+Conservation+Partnership"))

# Combine all downloaded data----
# Your data is now downloaded into many folders within the 'Downloads' folder. (You can open File Explorer or use the Files Pane to check)
# The below code will go into each of these folders and find all files that have the same ending (e.g. "_Metadata.csv") and bind them together.
# The end product is three data frames; metadata, maxn and length.

metadata <-ga.list.files("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_csv(.))%>% # combine into dataframe
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  glimpse()

unique(metadata$project) # check the number of projects in metadata
unique(metadata$campaignid) # check the number of campaigns in metadata

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
maxn<-ga.create.maxn()%>%
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.count=="Yes")%>%
  dplyr::filter(maxn>0)

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

## Combine Length, Lengths and 3D point files into length3dpoints----
length3dpoints<-ga.create.length3dpoints()%>%
  select(-c(time,project,comment))%>% # take time out  there is also a time column in the metadata
  dplyr::inner_join(metadata)%>%
  dplyr::filter(successful.length=="Yes")%>%
  glimpse()

## Save length files ----
setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

## Habitat -----
habitat <-ga.list.files("_Habitat.point.score.txt")%>%
  purrr::map_df(~ga.read.files_txt(.))%>%
  glimpse()

# Save Habitat file ----
setwd(staging.dir)
write.csv(habitat,paste(study,"habitat.csv",sep="_"),row.names = FALSE)


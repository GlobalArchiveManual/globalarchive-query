### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Project
# 3. combine multiple Campaigns into single Metadata, Count and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository


rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
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

# Three subfolders will be created within the 'working directory'. They are 'Downloads','Data to be checked' and 'Tidy data'

# The 'Downloads' folder saves files downloaded from GlobalArchive.

# The 'Data to be checked' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)

# **The only folder you will need to create is your working directory**

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
to.be.checked.dir<-paste(working.dir,"Data to be checked",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")

## Delete Downloads folder ----
# It will delete any data sitting within your 'Downloads' folder 
# DO NOT SAVE ANY OTHER FILES IN YOUR DOWNLOADS FILE
# After running this line they will not be recoverable
# This avoids doubling up GlobalArchive files, or including files from other Projects.
setwd(working.dir)
unlink(download.dir, recursive=TRUE)

## Create Downloads, Data to be checked and Tidy data folders ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Data to be checked"))
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

# In this example we are searching for a PROJECT called BIOL4408 Marine Ecology field trip"
# NOTE: change any spaces in the project name to '+'

ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, 
                                    q=ga.query.project("BIOL4408+Marine+Ecology+field+trip"))

# Combine all downloaded data----
## Metadata files ----
metadata <-ga.list.files("Metadata.csv")%>%
  purrr::map_df(~ga.read.files_csv(.))%>%
  glimpse()

## Count fles ----
count <-ga.list.files("Count.csv")%>%
  purrr::map_df(~ga.read.files_csv(.))%>%
  glimpse()

## Length files ----
length <-ga.list.files("Length.csv")%>%
  purrr::map_df(~ga.read.files_csv(.))%>%
  glimpse()

# Add metadata to count and add in zeros ----
complete.count<-as.data.frame(count)%>%
  tidyr::complete(nesting(project,campaignid,sample,family,genus,species),legal.sublegal,fill = list(count = 0))%>%
  dplyr::left_join(.,metadata)

## Save metadata, count and length files ----
setwd(to.be.checked.dir)

write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)
write.csv(count,paste(study,"count.csv",sep="_"),row.names = FALSE)
write.csv(length,paste(study,"length.csv",sep="_"),row.names = FALSE)
write.csv(complete.count,paste(study,"complete.count.csv",sep="_"),row.names = FALSE)

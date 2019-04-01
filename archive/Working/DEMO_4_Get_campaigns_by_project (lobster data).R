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
study<-"lobster.example"

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
API_USER_TOKEN <- "95933aec4289dd17cfb5e3ceee841e8f3f619f56d5dce96492566401"

# Set up your search criteria ----
# A number of example search criterias are given in the read me on the 'globalarchive-query' github repository.
# See: https://github.com/GlobalArchiveManual/globalarchive-query
# In this example we are searching for a PROJECT called "BIOL4408 Marine Ecology field trip"
# NOTE: change spaces in name to '+'

q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"BIOL4408+Marine+Ecology+field+trip"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Metadata files ----
metadata <-list.files.GA("Metadata.csv")%>%
  map_df(~read_files_csv(.))%>%
  glimpse()

## Count fles ----
count <-list.files.GA("Count.csv")%>%
  map_df(~read_files_csv(.))%>%
  glimpse()

## Length files ----
length <-list.files.GA("Length.csv")%>%
  map_df(~read_files_csv(.))%>%
  glimpse()

# Add metadata to count and add in zeros ----
complete.dat<-as.data.frame(count)%>%
  tidyr::complete(nesting(project,campaignid,sample,family,genus,species),legal.sublegal,fill = list(count = 0))%>%
  left_join(.,metadata)

## Save metadata, count and length files ----
setwd(data.dir)

write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)
write.csv(count,paste(study,"count.csv",sep="_"),row.names = FALSE)
write.csv(length,paste(study,"length.csv",sep="_"),row.names = FALSE)

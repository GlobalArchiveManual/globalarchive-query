rm(list=ls()) # Clear memory

# libraries ----
library(devtools)
library(dplyr)
install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)
library(httr)
library(jsonlite)
library(plyr)
library(purrr)
library(RCurl)
library(readr)
library(R.utils)
library(stringr)
library(tidyr)

### Study name ---
study<-"lobster.example"

### Set your working directory ----
working.dir<-"C:/GitHub/BIOL4408"
setwd(working.dir)

## Save directory names ----
data.dir=paste(working.dir,"Data",sep="/")
download.dir<-paste(working.dir,"Downloads",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

unlink(download.dir, recursive=TRUE) # Clear downloads folder (this will delete everything in the downloads folder (very scary))

## Create a folder for downloaded data and tidy data ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Data"))
dir.create(file.path(data.dir, "Tidy data"))
dir.create(file.path(data.dir, "Temporary data"))

# Bring in some consistent values used to download ----
source("https://raw.githubusercontent.com/GlobalArchiveManual/globalarchive-query/master/values.R")

### Setup your query ----
# API
API_USER_TOKEN <- "fba67680725035ebaf72e60db290933dc878454e2dd8506ec75085b1"

# API search by Project (space replaced with +) ----
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

# Add metadata to count ----
dat<-as.data.frame(count)%>%
  tidyr::complete(nesting(project,campaignid,sample,family,genus,species),legal.sublegal,fill = list(count = 0))%>%
  left_join(.,metadata)

## Save maxn and length files ----
setwd(tidy.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)
write.csv(count,paste(study,"count.csv",sep="_"),row.names = FALSE)
write.csv(length,paste(study,"length.csv",sep="_"),row.names = FALSE)
rm(list=ls()) # Clear memory

# Load Libraries ----
library(httr)
library(jsonlite)
library(RCurl)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(plyr)
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)
library(R.utils)

# Some url patterns for querying ----
URL_DOMAIN <- "https://globalarchive.org"
API_ENDPOINT_CAMPAIGN_LIST <- "/api/campaign"
API_ENDPOINT_CAMPAIGN_DETAIL <- "/api/campaign-full/%s"
API_ENDPOINT_CAMPAIGN_FILE <- "/api/campaign_file_file/%s"

# Study name ---
study<-"example.data.types"

### Set your working directory ----
working.dir<-("C:/GitHub/globalarchive-query") # This is the only folder you will need to create outside of R
setwd(working.dir)

## Save directory names ----
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Data/Tidy data",sep="/")

unlink(download.dir, recursive=TRUE) # Clear downloads folder (this will delete everything in the downloads folder (very scary)), DO NOT save anything in this file that is not downloaded using the query as it will be deleted and is not recoverable

## Create a folder for downloaded data and tidy data ----
data.dir=paste(working.dir,"Data",sep="/")
download.dir<-paste(working.dir,"Downloads",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

### Setup your query ----
# API
API_USER_TOKEN <- "b581a9ed9a2794010dd5edb4d68f214a81990d1645c4e3ad4caad0dd"

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Collaboration/Workgroup (space replaced with +) ----
q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"eq","val":"Example:+merging+different+data+types"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Metadata files ----
metadata <-list.files(path=download.dir,
                      recursive=T,
                      pattern="Metadata.csv",
                      full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  glimpse()

info.files <- list.files(path=download.dir,
                         recursive=T,
                         pattern=".info.csv",
                         full.names=T) 

info.files$lines<-sapply(info.files,countLines)

## Eventmeasure files ----
## Points files ----
points <-list.files(path=download.dir,
                    recursive=T,
                    pattern="_Points.txt",
                    full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>%
  glimpse()

## 3D Points files ----
threedpoints.files <-list.files(path=download.dir,
                                recursive=T,
                                pattern="3DPoints.txt",
                                full.names=T) # create a vector with all 3d point files that need to be read in 

threedpoints.files$lines<-sapply(threedpoints.files,countLines)

threedpoints<-as.data.frame(threedpoints.files)%>%
  mutate(campaign=row.names(.))%>%
  filter(lines>1)%>% # filter out all empty text files
  select(campaign)%>%
  as_vector(.)%>%
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,range,number,comment)%>%
  glimpse() 

## Lengths files ----
length.files <-list.files(path=download.dir,
                          recursive=T,
                          pattern="Lengths.txt",
                          full.names=T) # create a vector with all lengths files that need to be read in 

length.files$lines<-sapply(length.files,countLines) # add a new column that counts the number of lines in each file

lengths<-as.data.frame(length.files)%>%
  mutate(campaign=row.names(.))%>%
  filter(lines>1)%>% # filter out all empty text files
  select(campaign)%>%
  as_vector(.)%>%  
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,length,range,number,comment)%>%
  glimpse()

### Generic Campaigns ----
## Count fles ----
count <-list.files(path=download.dir,
                     recursive=T,
                     pattern="Count.csv",
                     full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(campaignid,sample,family,genus,species,count)%>%
  glimpse()

## Length files ----
length <-list.files(path=download.dir,
                     recursive=T,
                     pattern="Length.csv",
                     full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(campaignid,sample,family,genus,species,length,count)%>%
  glimpse()

## Make Maxn and length dataframes----
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
  
## Save maxn and length files ----
setwd(temp.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

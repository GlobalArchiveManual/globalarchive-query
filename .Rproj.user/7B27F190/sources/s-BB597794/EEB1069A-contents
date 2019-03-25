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

# Some url patterns for querying ----
URL_DOMAIN <- "https://globalarchive.org"
# URL_DOMAIN <- "http://localhost:5000"
API_ENDPOINT_CAMPAIGN_LIST <- "/api/campaign"
API_ENDPOINT_CAMPAIGN_DETAIL <- "/api/campaign-full/%s"
API_ENDPOINT_CAMPAIGN_FILE <- "/api/campaign_file_file/%s"

# Set Study Name ----
study<-"project.example"

### Set your working directory ----
working.dir<-("C:/GitHub/globalarchive-query") # This is the only folder you will need to create outside of R
setwd(working.dir)

## Save directory names ----
data.dir=paste(working.dir,"Data",sep="/")
download.dir<-paste(working.dir,"Downloads",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

unlink(download.dir, recursive=TRUE) # Clear downloads folder (this will delete everything in the downloads folder (very scary))

## Create a folder for downloaded data and tidy data ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Tidy data"))

### Setup your query ----
# API
API_USER_TOKEN <- "b581a9ed9a2794010dd5edb4d68f214a81990d1645c4e3ad4caad0dd"

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Project (space replaced with +) ----
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Pilbara+Marine+Conservation+Partnership"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Annotation info ----
# info<-list.files(path=download.dir,
#                  recursive=T,
#                  pattern="_info.csv",
#                  full.names=T) %>% 
#   map_df(~read_files_csv(.))%>%
#   select(campaignid,name,value)%>%
#   tidyr::spread(name,value)

read_files_txt()

## Metadata files ----
metadata <-list.files(path=download.dir,
             recursive=T,
             pattern="Metadata.csv",
             full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  #left_join(info)%>% # Join in annotation info
  glimpse()

name<-as.character(unique(metadata$project))

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
threedpoints <-list.files(path=download.dir,
             recursive=T,
             pattern="3DPoints.txt",
             full.names=T) %>%
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,range,number,comment)%>%
  glimpse()

## Lengths files ----
lengths <-list.files(path=download.dir,
             recursive=T,
             pattern="Lengths.txt",
             full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,length,range,number,comment)%>%
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
  filter(!maxn==0)%>%
  inner_join(metadata)%>%
  filter(successful.count=="Yes")%>%
  glimpse()

length3dpoints<-lengths%>%
  plyr::rbind.fill(threedpoints)%>%
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

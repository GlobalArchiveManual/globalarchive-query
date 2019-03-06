rm(list=ls()) # Clear memory

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

### Set your working directory ----
working.dir<-("C:/GitHub/globalarchive-query") # This is the only folder you will need to create outside of R
setwd(working.dir)

## Save directory names ----
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Data/Tidy data",sep="/")

unlink(download.dir, recursive=TRUE) # Clear downloads folder (this will delete everything in the downloads folder (very scary))

## Create a folder for downloaded data and tidy data ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Tidy data"))

### Setup your query ----
# API
API_USER_TOKEN <- "b581a9ed9a2794010dd5edb4d68f214a81990d1645c4e3ad4caad0dd" # tims for the moment

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

#Search for all campaigns matching pattern ( % = wildcard)
q='{"filters":[{"name":"name","op":"like","val":"%PSGLMP%"}]}' # % on either side can be anything - this gets all campaigns that contain "PSGLMP" in the campaign name 

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Metadata files ----
metadata <-list.files(path=download.dir,
                      recursive=T,
                      pattern="Metadata.csv",
                      full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  #left_join(info)%>% # Join in annotation info
  glimpse()

unique(metadata$campaignid)
name<-as.character(unique(metadata$project))

# Use metadata to make a list to re-save info with campagn name 
uniq.campaign <- unique(unlist(metadata$campaignid))
info.join<- data.frame()

info.join<-c("Campaignid","Project")%>%
  map_dfr( ~tibble(!!.x := logical() ) )

for (i in 1:length(uniq.campaign)){
  # Subset metadata to get project info
  metadata.sub <- subset(metadata, campaignid == uniq.campaign[i])
  
  remove.commas <- function(c){( gsub(",", '.', c))}
  remove.colon <- function(c){( gsub(";", '.', c))}
  remove<-function(c){( gsub(" ", " ", c))}
  
  project<-as.character(unique(metadata.sub$project)) # save project name
  campaignid<-as.character(unique(metadata.sub$campaignid))
  
  # set wd
  setwd(paste(download.dir,project,campaignid,sep="/"))
  
  # read in info csv 
  info<-read.csv(".info.csv")
  info[] <- sapply(info, remove.commas)
  info[] <- sapply(info, remove.colon)
  info[] <- sapply(info, remove)
  
  df <- data.frame(info)
  df$campaignid<-as.character(campaignid)
  info.join <- rbind(info.join,df)
  
  # write info with campaign name
  write.csv(info, file=paste(campaignid,"info.csv",sep="_"), quote=FALSE,row.names = FALSE)
}

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
  glimpse() # to ask tim how to do this better

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
write.csv(maxn,paste(name,"maxn.csv",sep="_"),row.names = FALSE)
write.csv(length3dpoints,paste(name,"length3dpoints.csv",sep="_"),row.names = FALSE)
write.csv(metadata,paste(name,"metadata.csv",sep="_"),row.names = FALSE)
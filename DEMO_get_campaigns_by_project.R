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

### Source functions----
galib <- getURL("https://raw.githubusercontent.com/UWAMEGFisheries/globalarchive-api/master/R/galib.R", ssl.verifypeer = FALSE)
eval(parse(text = galib))

functions <-getURL("https://raw.githubusercontent.com/GlobalArchiveManual/globalarchive-query/master/Functions.R", ssl.verifypeer = FALSE)
eval(parse(text = functions))

### Set your working directory ----
working.dir<-("C:/GitHub/globalarchive-query")
setwd(working.dir)

## Create a folder for downloaded data and tidy data ----
dir.create(file.path(working.dir, "Downloads"), showWarnings = TRUE) # Will warn if directory already exist
dir.create(file.path(working.dir, "Tidy data"), showWarnings = TRUE) # Will warn if directory already exist

## Save directory names ----
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")

### Setup your query ----
# API search by Project (space replaced with +) ----
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Pilbara+Marine+Conservation+Partnership"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Annotation info ----
info<-list.files(path="ProjectQuery",
                 recursive=T,
                 pattern="_info.csv",
                 full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  select(campaignid,name,value)%>%
  tidyr::spread(name,value)

## Metadata files ----
metadata <-list.files(path="ProjectQuery",
             recursive=T,
             pattern="Metadata.csv",
             full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::select(campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  left_join(info)%>% # Join in annotation info
  glimpse()

## Points files ----
points <-list.files(path="ProjectQuery",
             recursive=T,
             pattern="_Points.txt",
             full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>%
  glimpse()

## 3D Points files ----
threedpoints <-list.files(path="ProjectQuery",
             recursive=T,
             pattern="3DPoints.txt",
             full.names=T) %>%
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,range,number,comment)%>%
  glimpse()

## Lengths files ----
lengths <-list.files(path="ProjectQuery",
             recursive=T,
             pattern="Lengths.txt",
             full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
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




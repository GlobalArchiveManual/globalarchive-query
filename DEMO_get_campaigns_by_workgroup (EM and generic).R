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
working.dir<-("C:/GitHub/globalarchive-query") # This is the only folder you will need to create outside of R
setwd(working.dir)

## Save directory names ----
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")

unlink(download.dir, recursive=TRUE) # Clear downloads folder (this will delete everything in the downloads folder (very scary)), DO NOT save anything in this file that is not downloaded using the query as it will be deleted and is not recoverable

## Create a folder for downloaded data and tidy data ----
dir.create(file.path(working.dir, "Downloads"))
dir.create(file.path(working.dir, "Tidy data"))

### Setup your query ----
# API
API_USER_TOKEN <- "ef231f61b4ef204d39f47f58cccadf71af250a365e314e83dbcb3b08"  # Change to demo user when received

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Collaboration/Workgroup (space replaced with +) ----
q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"eq","val":"Example:+merging+different+data+types"}}]}'

# Name of the collaboration to save files at the end ----
name<-"Example merging different data types"

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Annotation info ----
info<-list.files(path=download.dir,
                 recursive=T,
                 pattern="_info.csv",
                 full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  select(campaignid,name,value)%>%
  tidyr::spread(name,value)

## Metadata files ----
metadata <-list.files(path=download.dir,
                      recursive=T,
                      pattern="Metadata.csv",
                      full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(project,campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  left_join(info)%>% # Join in annotation info
  glimpse()

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
  dplyr::select(-c(count))
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  inner_join(metadata)%>%
  filter(successful.length=="Yes")%>%
  glimpse()
  
## Save maxn and length files ----
setwd(tidy.dir)
write.csv(maxn,paste(name,"maxn.csv",sep="_"),row.names = FALSE)
write.csv(length3dpoints,paste(name,"length3dpoints.csv",sep="_"),row.names = FALSE)
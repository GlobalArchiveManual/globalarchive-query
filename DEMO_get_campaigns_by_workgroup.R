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

### Setup your query ----
API_USER_TOKEN <- "ef231f61b4ef204d39f47f58cccadf71af250a365e314e83dbcb3b08"  # Change to demo user when received

# This is the location where the downloaded data will sit ----
DATA_DIR <- "WorkgroupQuery"

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Collaboration/Workgroup (space replaced with +) ----
q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"eq","val":"Example:+merging+different+data+types"}}]}'

### Run the query and process the campaigns. Files will be downloaded into DATA_DIR ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

## Annotation info ----
info<-list.files(path="WorkgroupQuery",
                 recursive=T,
                 pattern="_info.csv",
                 full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  select(campaignid,name,value)%>%
  tidyr::spread(name,value)

## Metadata files ----
metadata <-list.files(path="WorkgroupQuery",
                      recursive=T,
                      pattern="Metadata.csv",
                      full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::select(campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  left_join(info)%>% # Join in annotation info
  glimpse()

## EventMeasure exports ----
## Points files ----
points <-list.files(path="WorkgroupQuery",
                    recursive=T,
                    pattern="_Points.txt",
                    full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>%
  glimpse()

## 3D Points files ----
threedpoints <-list.files(path="WorkgroupQuery",
                          recursive=T,
                          pattern="3DPoints.txt",
                          full.names=T) %>%
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,range,number,comment)%>%
  glimpse()

## Lengths files ----
lengths <-list.files(path="WorkgroupQuery",
                     recursive=T,
                     pattern="Lengths.txt",
                     full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  tidyr::separate(campaign.naming,into=c("folder","project","campaignid","file"),sep="/")%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,length,range,number,comment)%>%
  glimpse()

### Generic Campaigns ----
## Count fles ----


## Length files ----


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




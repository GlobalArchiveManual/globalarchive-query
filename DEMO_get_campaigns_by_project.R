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

# # Clean names function ----
# #TJL - this could be loaded from a repo as function above?
# clean_names <- function(dat){
#   old_names <- names(dat)
#   new_names <- old_names %>%
#     gsub("%", "percent", .) %>%
#     make.names(.) %>%
#     gsub("[.]+", ".", .) %>%
#     tolower(.) %>%
#     gsub("_$", "", .)
#   setNames(dat, new_names)
# }
# 
# ## Function that reads in csv files and creates a column for filepath to get CampaignID ----
# #TJL - this could be loaded from a repo as function above?
# read_files_csv <- function(flnm) {
#   read_csv(flnm,col_types = cols(.default = "c"))%>% 
#     mutate(campaignnames = flnm)%>%
#     separate(campaignnames,into=c("Folder","Project","CampaignID","File"),sep="/")%>% #"Synthesis",
#     select(-c(Folder,File))%>% #Synthesis,
#     clean_names
# }
# ## Function that reads in txt files and creates a column for filepath to get CampaignID ----
# #TJL - this could be loaded from a repo as function above?
# read_files_txt <- function(flnm) {
#   read_tsv(flnm,col_types = cols(.default = "c"))%>% 
#     mutate(campaignnames = flnm)%>%
#     separate(campaignnames,into=c("Folder","Project","CampaignID","File"),sep="/")%>% #"Synthesis",
#     select(-c(Folder,File))%>%#Synthesis,
#     clean_names
# }

### Setup your query ----
API_USER_TOKEN <- "ef231f61b4ef204d39f47f58cccadf71af250a365e314e83dbcb3b08"  # Change to demonstration user when receive it from ari

# This is the location where the downloaded data will sit ----
DATA_DIR <- "Data"

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Project (space replaced with +) ----
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Pilbara+Marine+Conservation+Partnership"}}]}'

# ### Return campaign objects ----
# #TJL - this could be loaded from a repo as function above?
# process_campaign_object <- function(object) {
#   # Perform another request to the API to get more detailed campaign info
#   campaign <- ga.get.campaign(API_USER_TOKEN, object["id"])
#   #print(toJSON(campaign, pretty=TRUE))  # show all avialable info
#   # Print campaign_info to console
#   ga.print.campaign_details(campaign)  # prints details about campaign
#   # Download/save campaign files and data
#   campaign_path <- file.path(DATA_DIR, campaign$project["name"], campaign$name) # create campaign path
#   dir.create(campaign_path, showWarnings = FALSE, recursive=TRUE)             # create campaign dir
#   campaign_files = ga.download.campaign_files(API_USER_TOKEN, campaign$files, campaign_path, match=MATCH_FILES)   # download all campaign files
#   ga.download.campaign_info(API_USER_TOKEN, campaign$info, campaign_path)     # generate csv file for info
#   ga.download.campaign_record(API_USER_TOKEN, campaign, campaign_path)        # generate json file containing campaign record information
# }

### Run the query and process the campaigns ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)
# The files are now downloaded into the folder selected above----

## Metadata files ----
metadata <-
  list.files(path="Data",
             recursive=T,
             pattern="Metadata.csv",
             full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  dplyr::select(campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  glimpse()

## Points files ----
points <-
  list.files(path="Data",
             recursive=T,
             pattern="_Points.txt",
             full.names=T) %>% 
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,number,frame)%>%
  glimpse()

## 3D Points files ----
threedpoints <-
  list.files(path="Data",
             recursive=T,
             pattern="3DPoints.txt",
             full.names=T) %>%
  map_df(~read_files_txt(.))%>%
  dplyr::rename(sample=opcode)%>%
  dplyr::select(campaignid,sample,family,genus,species,range,number,comment)%>%
  glimpse()

## Lengths files ----
lengths <-
  list.files(path="Data",
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




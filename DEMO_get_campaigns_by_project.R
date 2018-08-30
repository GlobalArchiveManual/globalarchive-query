library(httr)
library(jsonlite)
library(RCurl)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)

### Source functions----
galib <- getURL("https://raw.githubusercontent.com/UWAMEGFisheries/globalarchive-api/master/R/galib.R", ssl.verifypeer = FALSE)
eval(parse(text = galib))

# Clean names function ----
clean_names <- function(dat){
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", ".", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, new_names)
}

## Function that reads in csv files and creates a column for filepath to get CampaignID ----
read_files_csv <- function(flnm) {
  read_csv(flnm,col_types = cols(.default = "c"))%>% 
    mutate(campaignnames = flnm)%>%
    separate(campaignnames,into=c("Folder","Synthesis","Project","CampaignID","File"),sep="/")%>%
    select(-c(Folder,Synthesis,File))%>%
    clean_names
}
## Function that reads in txt files and creates a column for filepath to get CampaignID ----
read_files_txt <- function(flnm) {
  read_tsv(flnm,col_types = cols(.default = "c"))%>% 
    mutate(campaignnames = flnm)%>%
    separate(campaignnames,into=c("Folder","Synthesis","Project","CampaignID","File"),sep="/")%>%
    select(-c(Folder,Synthesis,File))%>%
    clean_names
}

### Setup your query ----
API_USER_TOKEN <- "ef231f61b4ef204d39f47f58cccadf71af250a365e314e83dbcb3b08"  # Change to demonstration user when receive it from ari
if (!exists("API_USER_TOKEN")) {
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)==0) {stop("Not API_USER_TOKEN found. Either set it in the code or pass it as an argument to the script!")}
  else {API_USER_TOKEN <- args[1]}   # get it from command line argument
}

# This is the location where the downloaded data will sit ----
DATA_DIR <- "Data"

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

# API search by Project (space replaced with +) ----
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Pilbara+Marine+Conservation+Partnership"}}]}'

### Return campaign objects ----
process_campaign_object <- function(object) {
  # Perform another request to the API to get more detailed campaign info
  campaign <- ga.get.campaign(API_USER_TOKEN, object["id"])
  #print(toJSON(campaign, pretty=TRUE))  # show all avialable info
  # Print campaign_info to console
  ga.print.campaign_details(campaign)  # prints details about campaign
  # Download/save campaign files and data
  campaign_path <- file.path(DATA_DIR, campaign$project["name"], campaign$name) # create campaign path
  dir.create(campaign_path, showWarnings = FALSE, recursive=TRUE)             # create campaign dir
  campaign_files = ga.download.campaign_files(API_USER_TOKEN, campaign$files, campaign_path, match=MATCH_FILES)   # download all campaign files
  ga.download.campaign_info(API_USER_TOKEN, campaign$info, campaign_path)     # generate csv file for info
  ga.download.campaign_record(API_USER_TOKEN, campaign, campaign_path)        # generate json file containing campaign record information
}

### Run the query and process the campaigns ----
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)

# The files are now downloaded into the folder selected on line 18 ----

## Bring in metadata files ----
metadata <-
  list.files(path="Data",
             recursive=T,
             pattern="Metadata.csv",
             full.names=T) %>% 
  map_df(~read_files_csv(.))%>%
  select(campaignid,sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length,comment)%>%
  glimpse()

names(metadata)









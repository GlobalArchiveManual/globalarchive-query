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
    mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/",remove=TRUE)%>%
    clean_names
}
## Function that reads in txt files and creates a column for filepath to get CampaignID ----
read_files_txt <- function(flnm) {
  read_tsv(flnm,col_types = cols(.default = "c"))%>% 
    mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/",remove=TRUE)%>%
    clean_names
}

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

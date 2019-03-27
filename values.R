# Some url patterns for querying ----
URL_DOMAIN <- "https://globalarchive.org"
API_ENDPOINT_CAMPAIGN_LIST <- "/api/campaign"
API_ENDPOINT_CAMPAIGN_DETAIL <- "/api/campaign-full/%s"
API_ENDPOINT_CAMPAIGN_FILE <- "/api/campaign_file_file/%s"

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"


# New functions here for the moment
list.files.GA<-function(pattern) {
  list.files(path=download.dir, # create a list of files that match the pattern "_Points.txt"
             recursive=T,
             pattern=pattern,
             full.names=T)
}


expand.files<-function(files) {
  as.data.frame(files)%>%
    mutate(campaign=row.names(.))%>%
    filter(lines>1)%>% # filter out all empty text files
    select(campaign)%>%
    as_vector(.)
}


read_files_txt <- function(flnm) {
  read_tsv(flnm,col_types = cols(.default = "c"))%>% 
    mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/",remove=TRUE)%>%
    clean_names%>%
    plyr::rename(., replace = c(opcode="sample"),warn_missing = TRUE)
}


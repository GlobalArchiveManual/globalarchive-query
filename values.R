# Some url patterns for querying ----
URL_DOMAIN <- "https://globalarchive.org"
API_ENDPOINT_CAMPAIGN_LIST <- "/api/campaign"
API_ENDPOINT_CAMPAIGN_DETAIL <- "/api/campaign-full/%s"
API_ENDPOINT_CAMPAIGN_FILE <- "/api/campaign_file_file/%s"

# This is the location where the downloaded data will sit ----
DATA_DIR <- download.dir

# Configure search pattern for downloading all files ----
MATCH_FILES <- ".csv$|.txt$"

#!/usr/bin/env Rscript

#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)
source("R/galib.R")

################################################################################
# Setup your query
################################################################################
# Set you GA API USER TOKEN for this query. If you do not set in the script, it
# expects to receive it from the command line argument
API_USER_TOKEN <- "ef231f61b4ef204d39f47f58cccadf71af250a365e314e83dbcb3b08"  # hard code it here or pass it as an argument
if (!exists("API_USER_TOKEN")) {
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)==0) {stop("Not API_USER_TOKEN found. Either set it in the code or pass it as an argument to the script!")}
  else {API_USER_TOKEN <- args[1]}   # get it from command line argument
}

# Configure this to be the location for which you want to download all
# campaign and project data for this query
DATA_DIR <- "Data"

# Configure search pattern for downloading all files
# Example: only download .csv and .txt files
MATCH_FILES <- ".csv$|.txt$"
# Example: only download files with "_Metadata."" in the filename
# MATCH_FILES <- "_Metadata."
# Example: download all files
# MATCH_FILES <- NULL

# Customise the API search criteria for your query (JSON)
# The examples below show some possible search queries. Many more types of
# queries are available. Note, only the last one will be used since it
# overwrites the previous ones, so comment out the ones you're not using
#TODO: build a function to help construct common queries
# EXAMPLE 1: search for all campaigns matching pattern ( % = wildcard)
# q='{"filters":[{"name":"name","op":"like","val":"%_PointAddis_stereoBRUVs"}]}'
# EXAMPLE 2: search for specific campaign by name
# q='{"filters":[{"name":"name","op":"eq","val":"2011-09_Barrow.PDS_stereoBRUVs"}]}'
# EXAMPLE 3: search for all campaigns by user's email
# q='{"filters":[{"name":"user","op":"has","val":{"name":"email","op":"eq","val":"euan.harvey@curtin.edu.au"}}]}'
# EXAMPLE 4: search for all campaigns from Project (note + for spaces)
q='{"filters":[{"name":"project","op":"has","val":{"name":"name","op":"eq","val":"Port+Stephens-Great+Lakes+Marine+Park+MER+Program"}}]}'

# EXAMPLE 5: search for all campaigns from Collaboration (note + for spaces)
# q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"eq","val":"Australian+BRUV+synthesis"}}]}'
# EXAMPLE 6: search for all campaigns from Collaboration with wildcard search (%=wildcarg, ilike=case insensitive)
# q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"ilike","val":"nsw%bruvs"}}]}'
# EXAMPLE 7: get all campaigns that my user account has access to
# q=""














################################################################################
# The following is an example of a user defined function that is passed into the
# API function to operate on the returned campaign objects. You can customise
# this to do whatever you want on each campaign that matches you search and will
# operate on each individually. If you want to work on all campaigns that match
# a search you can maintain data in data frames with shared scope.
################################################################################
process_campaign_object <- function(object) {
  #print(toJSON(object, pretty=TRUE))  # show available info from object list

  # Perform another request to the API to get more detailed campaign info
  campaign <- ga.get.campaign(API_USER_TOKEN, object["id"])
  #print(toJSON(campaign, pretty=TRUE))  # show all avialable info

  # Print campaign_info to console
  ga.print.campaign_details(campaign)  # prints details about campaign

  # Download/save campaign files and data
  campaign_path <- file.path(DATA_DIR, campaign$project["name"], campaign$name) # create campaign path: DATA_DIR/<project>/<campaign>
  dir.create(campaign_path, showWarnings = FALSE, recursive=TRUE)             # create campaign dir (if doesn't already exist)
  campaign_files = ga.download.campaign_files(API_USER_TOKEN, campaign$files, campaign_path, match=MATCH_FILES)   # download all campaign files
  ga.download.campaign_info(API_USER_TOKEN, campaign$info, campaign_path)     # generate csv file containing all campaign info properties
  ga.download.campaign_record(API_USER_TOKEN, campaign, campaign_path)        # generate json file containing campaign record information
  #print(campaign_files)  # prints output of campaign files including saved location
}




















################################################################################
# Run the query and process the campaigns
################################################################################
# This is where all the magic happens. It makes the API request to retrieve the
# campaigns matching the query "q" and then processes each one using the
# function pointer "process_campaign_object"
nresults <- ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, q=q)












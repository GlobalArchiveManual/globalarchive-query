## Combine "Campaign Information" ----
# Combine extra information fields saved on GlobalArchive e.g. Sampling Method, Image Analysis Method, etc...

uniq.campaign <- unique(unlist(metadata$campaignid)) # Use metadata to make a list to re-save info with campaign name 

info.join<- data.frame()%>% # create a blank dataframe with campaignid and project
  c("campaignid","project")%>%
  map_dfr( ~tibble(!!.x := logical() ) )

for (i in 1:length(uniq.campaign)){
  metadata.sub <- subset(metadata, campaignid == uniq.campaign[i]) # Subset metadata to get project info
  project<-as.character(unique(metadata.sub$project)) # save project name
  campaignid<-as.character(unique(metadata.sub$campaignid)) # save campaign name
  setwd(paste(download.dir,project,campaignid,sep="/")) # set wd
  info<-read.csv(".info.csv") # read in info csv 
  df <- data.frame(info)%>% # make a dataframe
    mutate(campaignid=as.character(campaignid))%>% # add in campaign id
    mutate(project=as.character(project)) # add in project
  info.join <- rbind(info.join,df) # add new dataframe to blank dataframe, loop will then add in the next campaign
}

# If this loop breaks it is because the info.csv downloaded is blank
# This means no extra "Campaign Information" has been saved.
# "Campaign Information" will need to be added to the campaign on GlobalArchive and then re-run line 78 (the line beginning with nresults)

# Re format the "Campaign Information" so each CampaignID is a row
info<-info.join%>%
  select(project,campaignid,name,value)%>%
  spread(.,name,value)

unique(metadata$campaignid)%>%sort() # Check the number of campaigns within metadata
unique(info$campaignid) # Check the number of campaigns with extra information
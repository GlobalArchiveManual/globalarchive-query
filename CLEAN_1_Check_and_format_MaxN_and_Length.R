### Check of MaxN and Length/3D points from EventMeasure data tables held in GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "globalarchive-query"

## This script is designed to be used in an interative process to suggest corrections that can/should be made to original EventMeasure or GlobalArchive files.

### OBJECTIVES ###
# 1. Import data and run BASIC data checks
# 2. Limit length data by range and precision rules
# 3. run SERIOUS checks against a master species list
# 4. Visualise what MaxN are missing in the stereoMaxN
# 5. Write data for analysis that passes checks

# Clear memory ----
rm(list=ls())

# Libraries required ----
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(data.table)
library(magrittr)
library(googlesheets)
library(ggplot2)
library(httpuv)
library(RCurl) # needed to download data from GitHub
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") # will not re-install if it hasn't been updated since last install
library(GlobalArchive) # if it errors try running again

## Set Study Name ----
# This needs to be the same as the study name set in the downloading script

#study<-"project.example" ## change for your project, this is continuing on from Demo_1
#study<-"workgroup.example" # Demo script 2
study<-"pattern.example" # Demo script 3

## File Structure ----
# This script works out of one main folder (this is called your 'working directory')
# There are five subfolders within the 'working directory'. They are 'Downloads','Data','Tidy data','Plots' and 'Errors to check'
# The 'Downloads' folder is used to save the unaltered files downloaded from GlobalArchive.
# The 'Data' folder is used to save the initial outputs from the downloading scripts (e.g. the combined metadata files) NOTE: These initial outputs have not gone through any validation steps (e.g. any of the checks against the life-history sheet)
# The 'Plots' folder is used to save initial checking plots
# The 'Tidy data' folder is used to save the final checked data
# The 'Errors to check' folder is used to save all the error files e.g. lists of taxa that are not in the life history sheet

# **The only folder you will need to create outside of R is your working directory**

### Set your working directory ----
# This also needs to be the same wokring directory used in the downloading script
working.dir<-("C:/GitHub/globalarchive-query")

setwd(working.dir)

## Save these sub directory names to use later----
plots.dir=paste(working.dir,"Plots",sep="/")
data.dir=paste(working.dir,"Data",sep="/") # made in the last script
tidy.dir=paste(working.dir,"Tidy data",sep="/")  # made in the last script
error.dir=paste(working.dir,"Errors to check",sep="/")

## Create a folder for Plots and Errors ----
# The two lines below will create the 'Plots' and 'Errors to check' subfolders within the working directory
dir.create(file.path(working.dir, "Plots"))
dir.create(file.path(working.dir, "Errors to check"))

# Bring in draft metadata ----
setwd(data.dir)
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  mutate(maxn=as.numeric(maxn))%>%
  mutate(species=tolower(species))%>%
  select(campaignid,sample,family,genus,species,maxn)%>%
  data.frame()%>%
  glimpse()

# Import length/3d file----
length<-read_csv(file=paste(study,"length3dpoints.csv",sep = "_"),na = c("", " "))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  select(campaignid,sample,family,genus,species,length,number,range)%>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  data.frame()%>%
  glimpse()

# Begin basic checks----
# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? 

# If there are no 3D points this will be blank

# Check if we have schools associated with single length measures----
schools<-length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? 

# If there are no schools with a single length measure this will be blank

# Standardise for RANGE and Error for Length ----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundance calculated from it, will be restricted by range

summary(length$range) # shows min, mean and max range
out.of.range<-filter(length,range>10000)%>%glimpse() # Shows fish more than 10 m away

# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,length>1000)%>%
  select(campaignid,sample,family,genus,species,length)%>%
  glimpse() # Shows fish that have a length measurement greater than 1 m (usually sharks, but should still be checked)

setwd(error.dir)
write.csv(fish.greater.than.1.meter,file=paste(study,"length.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)

# Further checks on fish bigger than their max length on fishbase are further down
# You should use the fish.greater.than.1.meter report to check for extreme outliers

# In this script there will be a .csv file for each error report
# NOTE: ALL ERRORS SHOULD BE FIXED IN THE EMOBs AND RE-UPLOADED TO GLOBAL ARCHIVE!

# Plot to visualise length data ----
# Plot length versus density
check.length<-ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
check.length # view the plot

# Save to plots file
setwd(plots.dir)
ggsave(check.length,file=paste(study,"check.length.png",sep = "_"))

# Plot to visualise range data ----
check.range<-ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2)
check.range # view the plot

# Save to plots file
setwd(plots.dir)
ggsave(check.range,file=paste(study,"check.range.png",sep = "_"))

# Plot to visualise length/range data ----
check.range.vs.length<-ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()

check.range.vs.length # view the plot

# Save to plots file
setwd(plots.dir)
ggsave(check.range.vs.length,file=paste(study,"check.range.vs.length.png",sep = "_"))

## Lump sp, spp, sp1, sp2 (etc) and other unknowns in species column to spp ----
# Add to this list to change species that should be lumped to spp

sp.list<-c(paste0("sp",1:20),paste0("sp ",1:20),paste0("sp.",1:20),paste0("sp. ",1:20),"sp","sp.","spp","species","unknown","NA","spp.")

sp.list # view what will be changed to spp

# For maxn - the function below turns all species that are included in the sp.list above to spp
# It groups by campaignid, sample and scientific name (fam, gen, spe)
# Changes all sp.list items to spp, and then if there are two with the same Genus and spp, chooses the highest
# e.g. if you have a drop with a maxn of 3 for Scarus sp1 and a maxn of 2 for Scarus sp2, your maxn of Scarus spp would be 3 (because Scarus sp1 has the highest maxn)
# The names changed can be viewed by looking at maxn.taxa.replaced.by.spp
maxn<-maxn.sp.to.spp(maxn,sp.list,return.changes = T)

# For length - The function replaces all species that are included in the sp.list to spp as well
# It sums the number of length measurements for each species per drop
# Then replaces the sp.list to spp
# Then only selects the one with the most length measurements if there are two with the same Genus spp
# The names changed can be viewed by looking at length.taxa.replaced.by.spp
length<-lengths.sp.to.spp(length,sp.list,return.changes = T)

# SERIOUS DATA CHECKING AGAINST AUSTRALIA LIFE HISTORY SHEET  ----
# Read in life history sheet
# Check for species recorded in areas that don't match their distribution
# Check for any species that may have changed names
#
# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
# or substitute your own life history sheet in here
# (names will need to match those used below)
# Make sure to select the correct Country and Marine Region, this is currently set up for the Demo_1 script

master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('NW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  distinct()%>%
  glimpse()

# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
synonyms <- gs_title("Synonyms_Australia")%>%
  gs_read_csv(ws = "Synonyms_Australia")%>%
  distinct()%>%
  clean_names()%>%
  select(-comment)

# Change synonyms ----
# This function will change synonyms in your data frame
# Use return.changes=T to view the taxa.replaced.by.synonym in R
# Use save.report to save .csv file in your error directory
maxn<-change.synonyms(maxn,return.changes=T,save.report = T)
length<-change.synonyms(length,return.changes=T,save.report = T)

# Check for taxa that do not have a match in the life history sheet ----
maxn.taxa.not.match.life.history<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>% 
  #distinct(campaignid,sample,family,genus,species)%>% # use this line to show specific drops
  distinct(family,genus,species)%>% # use this line to keep only fam, gen, spe
  filter(!species%in%c("spp"))%>% # Want to keep spps in (and not see them in the report)
  glimpse()

# CSV report
setwd(error.dir)
write.csv(maxn.taxa.not.match.life.history,file=paste(study,"maxn.taxa.not.match.life.history.csv",sep = "_"), row.names=FALSE)

# Length 
length.taxa.not.match<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(campaignid,sample,family,genus,species)%>%
  filter(!species%in%c("spp"))%>% # Want to keep spps in (and not see them in the report)
  glimpse()

# CSV report
setwd(error.dir)
write.csv(length.taxa.not.match,file=paste(study,"length.taxa.not.match.life.history.csv",sep = "_"), row.names=FALSE)

# Remove taxa that don't match from the final data ----
# This step can be skipped

maxn<-anti_join(maxn,maxn.taxa.not.match.life.history)
length<-anti_join(maxn,length.taxa.not.match)

## Check for Min Max Length compared to Master list----
# Mean max length for each family in the master list ----
family.max.length<-master%>%
  replace_na(list(fb.length_max=0))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max),na.rm = T)%>%
  dplyr::filter(!famlength_max==0)%>%
  select(-na.rm)

# Create a new master list with family mean max where missing maximum length ----
# (you can also replace all "family" with "genus" to create a genus average)
master.with.fam.max<-left_join(master,family.max.length,by=c("family"))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max))

wrong.length.taxa<-left_join(length,master.with.fam.max,by=c("family","genus","species"))%>%
  dplyr::filter(length >= fb.length_max)%>%
  dplyr::select(campaignid,sample,family,genus,species,length,fb.length_max,fb.ltypemaxm)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))%>%
  glimpse()

# For CSV report
setwd(error.dir)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)

## Drop wrong lengths ----
drop.length<-wrong.length.taxa%>% # TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(family,genus,species,length)%>%
  dplyr::select(family,genus,species,length)%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))

# Do not run this if you would like to keep those in wrong.length.taxa ----
length<-length%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))%>%
  anti_join(drop.length,by="key")%>% # for dropping wrong.lengths
  dplyr::select(-c(key))%>%
  glimpse()

# Check how many MaxN per species are missing from StereoMaxN, e.g. how many lengths are missing from the possible MaxN ----
# can only look at samples where lengths were possible
length.sample <- length%>%distinct(campaignid,sample)

# summarise length and then compare to maxn
taxa.maxn.vs.stereo.summary<-length%>%
  group_by(campaignid,sample,family,genus,species)%>%
  dplyr::summarise(stereo.maxn=sum(number))%>%
  full_join(maxn)%>%
  replace_na(list(maxn=0))%>%
  filter(!stereo.maxn==maxn)%>%
  mutate(percent.difference = (maxn-stereo.maxn)/maxn*100)%>%
  semi_join(length.sample)%>% # only keep ones where length was possible
  replace_na(list(percent.difference=1))%>%
  filter(!percent.difference%in%c(0))%>%
  mutate(difference=(maxn-stereo.maxn))%>%
  mutate(difference=abs(difference))%>%
  mutate(percent.difference=abs(percent.difference))%>%
  select(campaignid,sample,family,genus,species,maxn,stereo.maxn,difference,percent.difference)%>%
  arrange(-difference)%>%
  glimpse()

# For CSV report
setwd(error.dir)
write.csv(taxa.maxn.vs.stereo.summary,file=paste(study,"taxa.maxn.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

# WRITE FINAL checked data----
setwd(tidy.dir)
dir()

write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "_"), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.csv",sep = "_"), row.names=FALSE)

# GO TO Cleaning script 2 

### Error checks of MaxN and Length files created from EventMeasure or generic stereo-video annotations via GlobalArchive

## This script is designed to be used interatively to suggest species name and length corrections that should be made to original EventMeasure (.EMObs) or generic annotation files AND for subsequent data analysis.

# NOTE: ERRORS SHOULD BE FIXED IN THE .EMObs AND RE-UPLOADED TO GLOBAL ARCHIVE!


### OBJECTIVES ###
# 1. Import data and run BASIC error reports
# 2. Limit length data by range and precision rules
# 3. run SERIOUS error reports against a master species list
# 4. Visualise what MaxN are missing in the stereoMaxN
# 5. Write data for analysis that passes checks

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

# Please email tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au if you would like the life.history or synonyms googlesheets shared with you or have your local species information added.


# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
# library(data.table)
# library(magrittr)
# library(RCurl) # needed to download data from GitHub

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"project.example" 

## Folder Structure ----
# This script uses one main folder ('working directory')

# Three subfolders will already be created within the 'working directory'. They are 'Downloads','Staging' and 'Tidy data'

# In addition, this script will make folder for:
#'Plots' to save initial checking plots
#'Errors to check' to save all the error files e.g. lists of taxa that are not in the life history sheet

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")

## Create a folder for Plots and Errors ----
# The two lines below will create the 'Plots' and 'Errors to check' subfolders within the working directory
dir.create(file.path(working.dir, "Plots"))
dir.create(file.path(working.dir, "Errors to check"))

# Import unchecked data from staging folder----
setwd(staging.dir)

# Import metadata ---
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  mutate(maxn=as.numeric(maxn))%>%
  mutate(species=tolower(species))%>%
  select(campaignid,sample,family,genus,species,maxn)%>%
  # data.frame()%>%
  glimpse()

# Import length/3d file----
length<-read_csv(file=paste(study,"length3dpoints.csv",sep = "_"),na = c("", " "))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  select(campaignid,sample,family,genus,species,length,number,range)%>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  # data.frame()%>%
  glimpse()

# BASIC checks----
# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? 

# Check if we have >1 fish associated with single length measures----
schools<-length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? 

# Standardise for RANGE and Error for Length ----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundance calculated from it, will be restricted by this Range

summary(length$range) # shows min, mean and max range
out.of.range<-filter(length,range>10000)%>%
  glimpse() # Shows fish more than 10 m away

# Plot to visualise length data ---
setwd(plots.dir)
ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
ggsave(file=paste(study,"check.length.png",sep = "_"))

# Plot to visualise range data ---
ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
ggsave(file=paste(study,"check.range.png",sep = "_"))


# Plot to visualise range vs length data ---
# note that only 3Dpoints typically occur past 10m---
ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()
ggsave(file=paste(study,"check.range.vs.length.png",sep = "_"))

# Check on the BIG fish in length data----
setwd(error.dir)
fish.greater.than.1.meter<-filter(length,length>1000)%>%
  select(campaignid,sample,family,genus,species,length)%>%
  glimpse() # Shows fish that have a length measurement greater than 1 m to check (usually sharks)
write.csv(fish.greater.than.1.meter,file=paste(study,"length.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)

# SERIOUS checks on fish length vs their max.length in life.history will be done below
# Use the fish.greater.than.1.meter report to check for extreme outliers

# SERIOUS data checks using the life.history googlesheet ----

# life.history checks will:
# 1. Check for species occurence vs their known distribution
# 2. Check for any species that may have changed names and suggest synonyms
# 3. Check measured length vs max.length for that species

# Make sure to select the correct Country and Marine Region
# currently set for the Pilbara, Australia example data set

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

synonyms <- gs_title("Synonyms_Australia")%>%
  gs_read_csv(ws = "Synonyms_Australia")%>%
  distinct()%>%
  clean_names()%>%
  select(-comment)

# Update by synonyms ----
# This function will standardise all taxa by their synonyms 
# Use return.changes=T to view the taxa.replaced.by.synonym
# Use save.report to save .csv file in your error directory
maxn<-ga.change.synonyms(maxn,return.changes=T,save.report = T)
length<-ga.change.synonyms(length,return.changes=T,save.report = T)

# Check MaxN species names vs. life.history ----
maxn.taxa.not.match.life.history<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>% 
  distinct(campaignid,sample,family,genus,species)%>% # use this line to show specific drops OR
  # distinct(family,genus,species)%>% # use this line to keep only fam, gen, spe
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(maxn.taxa.not.match.life.history,file=paste(study,"maxn.taxa.not.match.life.history.csv",sep = "."), row.names=FALSE)

# Check Length species names vs. life.history ----
length.taxa.not.match<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(campaignid,sample,family,genus,species)%>%
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(length.taxa.not.match,file=paste(study,"length.taxa.not.match.life.history.csv",sep = "."), row.names=FALSE)

# Check Length vs. max.length in life.history----
# 1. Create mean max length for each family---
family.max.length<-master%>%
  replace_na(list(fb.length_max=0))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max),na.rm = T)%>%
  dplyr::filter(!famlength_max==0)%>%
  select(-na.rm)

# 2. Create a new master list with family mean max where missing species max.length ----
# (you can also replace all "family" with "genus" to create a genus average)
master.with.fam.max<-left_join(master,family.max.length,by=c("family"))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max))%>%
  glimpse()

# 3. Create list of wrong length, ordered by %error, for checking in .EMObs files or removing from data----
wrong.length.taxa<-left_join(length,master.with.fam.max,by=c("family","genus","species"))%>%
  dplyr::filter(length >= fb.length_max)%>%
  dplyr::select(campaignid,sample,family,genus,species,length,fb.length_max,fb.ltypemaxm)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))%>%
  glimpse()

setwd(error.dir)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)

# Check MaxN per species/family vs. StereoMaxN, e.g. how many lengths are missing from the  MaxN ----

length.sample <- length%>%distinct(campaignid,sample) # only examine samples where lengths were possible

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
  filter(!percent.difference%in%c(0))%>% #only for those that have missing lengths
  mutate(difference=(maxn-stereo.maxn))%>%
  mutate(difference=abs(difference))%>%
  mutate(percent.difference=abs(percent.difference))%>%
  select(campaignid,sample,family,genus,species,maxn,stereo.maxn,difference,percent.difference)%>%
  arrange(-difference)%>%
  glimpse()

setwd(error.dir)
write.csv(taxa.maxn.vs.stereo.summary,file=paste(study,"taxa.maxn.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

ggplot(taxa.maxn.vs.stereo.summary,aes(x=maxn,y=stereo.maxn,label = species))+
  geom_abline(colour="red",alpha=0.5)+
  geom_point()+
  geom_text(alpha=0.2)
setwd(plots.dir)
ggsave(file=paste(study,"check.stereo.vs.maxn.png",sep = "_"))

# NOW check through the files in your "Errors to check" folder and make corrections to .EMObs / generic files and then re-run this script.
# IF you are happy to proceed by dropping the speices, length and range errors here you can run the lines below and write the checked data 

# Drop errors from data----

# CAUTION Standardise by Range if you wish  ----
length<-length%>%
  filter(range<10000)%>%
  glimpse()

# CAUTION Remove taxa that don't match from the final data ----
maxn<-anti_join(maxn,maxn.taxa.not.match.life.history)
length<-anti_join(length,length.taxa.not.match)

# CAUTION Drop wrong lengths ----
drop.length<-wrong.length.taxa%>% # TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(family,genus,species,length)%>%
  dplyr::select(family,genus,species,length)%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))

length<-length%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))%>%
  anti_join(drop.length,by="key")%>% # for dropping wrong.lengths
  dplyr::select(-c(key))%>%
  glimpse()


# WRITE FINAL checked data----
setwd(tidy.dir)
dir()
write.csv(metadata, file=paste(study,"checked.metadata.csv",sep = "."), row.names=FALSE)
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.csv",sep = "."), row.names=FALSE)

# Go to FORMAT script (3) 

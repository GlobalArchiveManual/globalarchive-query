
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

# Please email tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au if you would like the life.history or synonyms googlesheets shared with you or to have your local species information added.


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

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"project.example"

## Folder Structure ----
# This script uses one main folder ('working directory')

# Three subfolders will already be created within the 'working directory'. They are 'Downloads','Data to be checked' and 'Tidy data' (Script 1)

# In addition, this script will make new folders for:
#'Plots' to save initial checking plots
#'Errors to check' to save all the error files e.g. lists of taxa that are not in the life history sheet

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to that of this script - or type your own

## Save these directory names to use later----
to.be.checked.dir<-paste(working.dir,"Data to be checked",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")

## Create a folder for Plots and Errors ----
# The two lines below will create the 'Plots' and 'Errors to check' subfolders within the working directory
dir.create(file.path(working.dir, "Plots"))
dir.create(file.path(working.dir, "Errors to check"))

# Import unchecked data from staging folder----
setwd(to.be.checked.dir)

# Import metadata ---
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  mutate(maxn=as.numeric(maxn))%>%
  mutate(species=tolower(species))%>%
  select(campaignid,sample,family,genus,species,maxn)%>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
  glimpse()

# Import length/3d file----
length<-read_csv(file=paste(study,"length3dpoints.csv",sep = "_"),na = c("", " "))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  select(campaignid,sample,family,genus,species,length,number,range)%>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
  mutate(genus=str_replace_all(.$genus,c("NA"="Unknown")))%>%
  glimpse()

# BASIC checks----
# Check if we have 3d points (Number) in addition to length----
# Use this check to identify any 3D points in your data. 3D points are often added if a length measurement couldn't be made.
# Should you have 3D points? 

three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? 

# Check if we have more than one fish associated with single length measurement----
# For large schools (e.g. 100+) a length measurement can be assigned to multiple individuals (by changing the 'Number' in EventMeasure). For example in a school of 500 if you measure 50 fish, and put '10' for each measurement in EM, you would have 500 fish but only 50 measurements
# Use this check to see if you have these types of measurements (e.g. to fix a mistaken number against multiple lengths).

schools<-length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? 


# Plot to visualise length data ----
# Add justification and units to x
setwd(plots.dir)

theme_ga<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Length vs. Density ----
# Use this plot to visualise the length distribution in your data
ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Length (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.length.png",sep = "_"))

# Range vs. Density ---
# Use this plot to visulaise the range distribution in your data
ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.range.png",sep = "_"))

# Plot to visualise Range vs Length data ---
# note that only 3Dpoints typically occur past 10m---
ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Length (mm)")+theme_ga

ggsave(file=paste(study,"check.range.vs.length.png",sep = "_"))


# Standardise for Range ----
# To standardise for Range we can remove any length observations outside Range rules
# i.e. the length data, and any abundance calculated from it, will be restricted by this Range

summary(length$range) # shows min, mean and max range

out.of.range<-filter(length,range>10000)%>% # 10 m = 10000 mm
  glimpse() # Shows fish more than 10 m away

# SERIOUS data checks using the life.history googlesheet ----
# Checks on fish length vs their max.length in the life.history sheet will be done below

# life.history checks will:
# 1. Check for species occurence vs their known distribution
# 2. Check for any species that may have changed names and suggest synonyms
# 3. Check measured length vs max.length for that species

# Make sure to select the correct Country and Marine Region that matches your data (see the two filter lines below)
# Follow this link to see a map of the marine regions used in the life history sheet
#  https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions

# These Marine Region abbreviations are:
# 'SW' - South-west
# 'NW' - North-west
# 'N' - North
# 'CS' - Coral Sea
# 'TE' - Temperate East
# 'SE' - South-east
# 'Christmas.Island' - Christmas Island
# 'Cocos.Keeling' - Cocos (Keeling) Island
# 'Lord.Howe.Island' - Lord Howe Island

# Use the abbreviation in the code below
# currently set for the Pilbara, Australia example data set ('NW' for North-west)

master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%ga.clean.names()%>%
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
  ga.clean.names()%>%
  select(-comment)

# Update by synonyms ----
# This function will change the names of species that have been reclassified (i.e. Pagrus auratus to Chrysophrys auratus). This function also fixes some common spelling mistakes (i.e. Chyrosophyrs	auratus to Chrysophrys auratus)

# Use return.changes=T to view the taxa.names.updated
# Use save.report to save .csv file in your error directory

maxn<-ga.change.synonyms(maxn,return.changes=T,save.report = T)
length<-ga.change.synonyms(length,return.changes=T,save.report = T)

# Check MaxN for species that have not previously been observed in your region ----
maxn.species.not.previously.observed<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>% 
  distinct(campaignid,sample,family,genus,species)%>% # use this line to show specific drops OR
  # distinct(family,genus,species)%>% # use this line to keep only fam, gen, spe
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(maxn.species.not.previously.observed,file=paste(study,"maxn.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length for species that have not previously been observed in your region ----
#maxn.species.not.previously.observed
length.species.not.previously.observed<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(campaignid,sample,family,genus,species)%>%
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(length.species.not.previously.observed,file=paste(study,"length.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length measurements vs. maximum length in life.history----
# 1. Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---

family.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max))%>%
  ungroup()

genus.max.length<-master%>%
  filter(!is.na(fb.length_max))%>%
  dplyr::group_by(genus)%>%
  dplyr::summarise(genuslength_max=mean(fb.length_max))%>%
  ungroup()

# 2. Create a new master list with family and genus average maximum length where missing species max.length ----
# In this example fish bigger than 85% of the maximum length recorded on FishBase will show up as an error ("too big").
# We think this is a better way to check lengths, because extremelly big individuals should be re-checked. For example on Fishbase the maximim length for Tiger Sharks (Galeocerdo cuvier) is 7.5 m, however we think we should really be checking any sharks bigger than 6 m rather than just those over 7.5 m.
# In this example the minimum length is 15% of the maximum on fishbase, fish that are smaller will show up as an error ("too small").
# you can change these values to be stricter (e.g. increase the minimum value and decrease the maximum) or more tolerant (decrease min and increase max) below.

master.min.max<-left_join(master,family.max.length,by=c("family"))%>% # add in family values
  left_join(.,genus.max.length)%>% # add in genus values
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),genuslength_max,fb.length_max))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max,genuslength_max))%>%
  mutate(min.length=0.15*fb.length_max)%>% # change values here
  mutate(max.length=0.85*fb.length_max)%>% # change values here
  glimpse()

# 3. Create list of wrong length, ordered by %error, for checking in .EMObs files or removing from data----
wrong.length.taxa<-left_join(length,master.min.max,by=c("family","genus","species"))%>%
  dplyr::filter(length<min.length|length>max.length)%>%
  mutate(reason=ifelse(length<min.length,"too small","too big"))%>%
  dplyr::select(campaignid,sample,family,genus,species,length,min.length,max.length,fb.length_max,reason)%>%
  mutate(difference=ifelse(reason%in%c("too small"),(min.length-length),(length-max.length)))%>%
  dplyr::mutate(percent.of.fb.max=(length/fb.length_max*100))%>%
  glimpse()

setwd(error.dir)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)

# You should view this .csv outside of R and revisit the .EMObs to check the lengths. Some of the length measurements could be correct.
# We suggest sorting OpCode and then checking and/or fixing each .EMObs file one at a time.

# Check MaxN per species/family vs. StereoMaxN, e.g. how many lengths are missing from the MaxN ----
# In this part we are checking the maxn (for each species) against the number measured (length and 3D point)
# this is important if you use the 3D measurements (3D points and length) for abundance 
# E.g. number above legal size

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

# Plot of maxn vs stereo maxn (length and 3D point)
ggplot(taxa.maxn.vs.stereo.summary,aes(x=maxn,y=stereo.maxn,label = paste(genus,species,sep=" ")))+
  geom_abline(colour="red",alpha=0.5)+
  geom_point()+
  geom_text(alpha=0.2)+theme_ga
setwd(plots.dir)
ggsave(file=paste(study,"check.stereo.vs.maxn.png",sep = "_"))

# We strongly encourage you to fix these errors at the source (i.e. EMObs), however, there may be observations that you want to keep in the raw data but not upload to Global Archive (i.e. seasnakes), that you can drop using the code below.
# NOW check through the files in your "Errors to check" folder and make corrections to .EMObs / generic files and then re-run this script.
# IF you are happy to proceed by removing the species, length and range errors here you can run the lines below and write the checked data 

# Drop errors from data ----

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

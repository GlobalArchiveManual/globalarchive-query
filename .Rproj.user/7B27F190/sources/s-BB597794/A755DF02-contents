### Check of MaxN and Length/3D points from EventMeasure data tables held in GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

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
library(dplyr)
library(readr)
library(data.table)
library(magrittr)
library(googlesheets)
library(ggplot2)
library(httpuv)
library(RCurl) # needed to download data from GitHub

# Study name---
study<-"Example" ## change for your project

# Set working directory ----
work.dir=("C:/GitHub/Format-EventMeasure-database-outputs") ## Change to your directory

# Set sub directories----
plots.dir=paste(work.dir,"Plots",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
export.dir=paste(data.dir,"Database output",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

# Load metadata ----
setwd(data.dir)
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))%>%
  setNames(tolower(names(.)))

# Load functions from GitHub----
gsr <- getURL("https://raw.githubusercontent.com/TimLanglois/Fuctions/master/gsr.r", ssl.verifypeer = FALSE)
eval(parse(text = gsr))
detach("package:RCurl", unload=TRUE) # will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() # TJL

# A function we should use throughout----
clean_names <- function(dat){
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", ".", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, new_names)
}

# Import MaxN and length/3d files----
setwd(temp.dir)

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  mutate(maxn=as.numeric(maxn))%>%
  data.frame()%>%
  glimpse()

# Import length/3d file----
length<-read_csv(file=paste(study,"Len3DPoints.csv",sep = "_"),na = c("", " "))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  data.frame()%>%
  glimpse()

# Begin basic checks----
setwd(data.dir)

# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? In this example - YES, if no 3D points will be empty

# Check if we have schools associated with single length measures----
schools<-length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? YES, if no schools will be empty


# Standardise for RANGE and Error for Length ----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundance calculated from it, will be restricted by range

summary(length$range) # shows min, mean and max range
out.of.range<-filter(length,range>10000)%>%glimpse() ## In this example there are no fish out of range (empty dataframe)

# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,length>1000)%>%
  glimpse() # Shows fish that have a length measurement greater than 1 m (usually sharks, but should be checked, but will come out again if larger than the max length in the life history sheet)

setwd(temp.dir)
write.csv(fish.greater.than.1.meter,file=paste(study,"check","length.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)

# Plot to visualise length data ----
setwd(plots.dir)

check.length<-ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
check.length # most fish less than 300mm in this example

ggsave(check.length,file=paste(study,"check.length.png",sep = "_"))

# Plot to visualise range data ----
check.range<-ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2)
check.range # most fish recorded under 2 m away

ggsave(check.range,file=paste(study,"check.range.png",sep = "_"))

# Plot to visualise length/range data ----
check.range.vs.length<-ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()
check.range.vs.length 

ggsave(check.range.vs.length,file=paste(study,"check.range.vs.length.png",sep = "_"))

# SERIOUS data checking to compare taxa to Master list and correct any names that may have changed----
# Read in species list to compare against----
# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
# or substitute your own life history sheet in here
master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('NW', marine.region))%>% # Select marine region (this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  distinct()%>%
  glimpse()

# Update names of species that may have changed, using synonyms list ----
# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
synonyms <- gs_title("Synonyms_Australia")%>%
  gs_read_csv(ws = "Synonyms_Australia")%>%
  distinct()%>%
  clean_names()%>%
  select(-comment)

# Change synonyms
maxn<-left_join(maxn,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))

length<-left_join(length,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))

# Check for taxa.not.match----
setwd(temp.dir)

# MaxN
maxn.taxa.not.match.life.history<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>%
  distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(maxn.taxa.not.match.life.history,file=paste(study,"check.maxn.taxa.not.match.life.history","csv",sep = "_"), row.names=FALSE)

# Length 
length.taxa.not.match<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(length.taxa.not.match,file=paste(study,"check.length.taxa.not.match.life.history","csv",sep = "_"), row.names=FALSE)

### SERIOUS Check for Min Max Length compared to Master list----
library(plyr)
names(master)

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
  dplyr::select(sample,family,genus,species,length,fb.length_max,fb.ltypemaxm)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))

setwd(temp.dir)
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
length.sample <- length%>%distinct(sample)

# summairse length and then compare to maxn
taxa.maxn.vs.stereo.summary<-length%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(stereo.maxn=sum(number))%>%
  left_join(maxn)%>%
  mutate(percent.diff = (maxn-stereo.maxn)/maxn)%>%
  semi_join(length.sample)%>% # only keep ones where length was possible
  replace_na(list(percent.diff=1))%>%
  filter(!percent.diff%in%c(0))%>%
  glimpse()

write.csv(taxa.maxn.vs.stereo.summary,file=paste(study,"taxa.maxn.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

# WRITE FINAL checked data----
setwd(tidy.dir)
dir()

write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "_"), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.csv",sep = "_"), row.names=FALSE)

# GO TO SCRIPT 4

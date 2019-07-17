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
library(stringr)
library(readr)
library(ggplot2)
library(fst)

# Search and replace function ----
gsr <- function(Source, Search, Replace) { 
  if (length(Search) != length(Replace))     stop("Search and Replace Must Have Equal Number of Items\n") 
  Changed <- as.character(Source) 
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
    Changed <- replace(Changed, Changed == Search[i], Replace[i])   } 
  cat("\n")    
  Changed 
}

# Study name---
study<-"project.example"  ## change for your project

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Read in metadata----
metadata<-read_csv(file=paste(study,"checked.metadata.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  glimpse()

# Read in habitat ----
## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
staging.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Read in metadata----
metadata<-read_csv(file=paste(study,"checked.metadata.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  glimpse()

# Read in habitat----
setwd(staging.dir)
habitat<-read_csv(file=paste(study,"habitat.csv",sep = "_"),na = c("", " ","NA",NA))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  select(sample,campaignid,project,id,everything())%>%
  glimpse()

names(habitat)%>%sort()

# Change habitat names to broad -----
# # Read in name changes to update names----
updated.names <- gs_title("Update.Habitat.Names")%>%
  gs_read_csv(ws = "Sheet1")%>%
  mutate(new.name=tolower(new.name))%>%
  glimpse()

# Update names----
colnames(habitat)<- gsr(colnames(habitat),updated.names$current.name,updated.names$new.name) # changes old names to new names
names(habitat)%>%sort()
habitat[is.na(habitat)] <- 0 # changes NAs to zeros
cols = c(5:66); 
names(habitat)%>%sort()
habitat[,cols] = apply(habitat[,cols], 2, function(x) as.numeric(as.character(x))); # changes all columns to numeric
habitat<-habitat%>%replace_na(cols = 0)

habitat<-habitat%>%
  # Relief
  mutate(relief.0 = habitat %>% select(starts_with("relief.0")) %>% rowSums())%>%
  mutate(relief.1 = habitat %>% select(starts_with("relief.1")) %>% rowSums())%>%
  mutate(relief.2 = habitat %>% select(starts_with("relief.2")) %>% rowSums())%>%
  mutate(relief.3 = habitat %>% select(starts_with("relief.3")) %>% rowSums())%>%
  mutate(relief.4 = habitat %>% select(starts_with("relief.4")) %>% rowSums())%>%
  mutate(relief.5 = habitat %>% select(starts_with("relief.5")) %>% rowSums())%>%
  # Field of view
  mutate('fieldofview.facing.down' = habitat %>% select(starts_with("fieldofview.facing.down")) %>% rowSums())%>%
  mutate('fieldofview.facing.up' = habitat %>% select(starts_with("fieldofview.facing.up")) %>% rowSums())%>%
  mutate('fieldofview.limited' = habitat %>% select(starts_with("fieldofview.limited")) %>% rowSums())%>%
  mutate('fieldofview.open' = habitat %>% select(starts_with("fieldofview.open")) %>% rowSums())%>%
  # Biota
  mutate('biota.ascidians' = habitat %>% select(starts_with("ascidians")) %>% rowSums())%>%
  mutate('biota.bryozoa' = habitat %>% select(starts_with("bryozoa")) %>% rowSums())%>%
  mutate('biota.consolidated' = habitat %>% select(starts_with("consolidated")) %>% rowSums())%>%
  mutate('biota.crinoids' = habitat %>% select(starts_with("crinoids")) %>% rowSums())%>%
  mutate('biota.hydrocoral' = habitat %>% select(starts_with("hydrocoral")) %>% rowSums())%>%
  mutate('biota.hydroids' = habitat %>% select(starts_with("hydroids")) %>% rowSums())%>%
  mutate('biota.invertebrate.complex' = habitat %>% select(starts_with("invertebrate.complex")) %>% rowSums())%>%
  mutate('biota.macroalgae' = habitat %>% select(starts_with("macroalgae")) %>% rowSums())%>% 
  mutate('biota.mangrove' = habitat %>% select(starts_with("mangrove")) %>% rowSums())%>% 
  mutate('biota.octocoral.black' = habitat %>% select(starts_with("octocoral.black")) %>% rowSums())%>%
  mutate('biota.open.water' = habitat %>% select(starts_with("open.water")) %>% rowSums())%>%
  mutate('biota.seagrasses' = habitat %>% select(starts_with("seagrasses")) %>% rowSums())%>%
  mutate('biota.sponges' = habitat %>% select(starts_with("sponges")) %>% rowSums())%>%
  mutate('biota.stony.corals' = habitat %>% select(starts_with("stony.corals")) %>% rowSums())%>%
  mutate('biota.true.anemones' = habitat %>% select(starts_with("true.anemones")) %>% rowSums())%>%
  mutate('biota.unconsolidated' = habitat %>% select(starts_with("unconsolidated")) %>% rowSums())%>%
  mutate('biota.zoanthids' = habitat %>% select(starts_with("zoanthids")) %>% rowSums())%>%
  select(id,sample,campaignid,project,relief.0,relief.1,relief.2,relief.3,relief.4,relief.5,fieldofview.facing.down,fieldofview.facing.up,fieldofview.limited,fieldofview.open,biota.ascidians,biota.bryozoa,biota.consolidated,biota.crinoids,biota.hydrocoral,biota.hydroids,biota.invertebrate.complex,biota.macroalgae,biota.mangrove,biota.octocoral.black,biota.seagrasses,biota.sponges,biota.stony.corals,biota.true.anemones,biota.unconsolidated,biota.zoanthids)

# Create %fov----
fov.percent.cover<-habitat%>%
  select(campaignid,sample,starts_with("fieldofview"))%>%
  mutate_all(funs(replace(.,is.na(.),0)))%>%
  mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  group_by(campaignid,sample)%>%
  mutate_at(vars(starts_with("fieldofview.")),funs(./Total.Sum*100))%>%
  dplyr::select(-Total.Sum)%>%
  glimpse()

# Create relief----
relief.mean.and.sd<-habitat%>%
  select(campaignid,sample,starts_with("relief."))%>%
  gather(key=relief,value=score,-campaignid,-sample)%>%
  filter(score>0)%>%
  mutate(relief.rank=ifelse(relief=="relief.0",0,ifelse(relief=="relief.1",1,ifelse(relief=="relief.2",2,ifelse(relief=="relief.3",3,ifelse(relief=="relief.4",4,ifelse(relief=="relief.5",5,relief)))))))%>%
  .[rep(seq.int(1,nrow(.)), .$score), 1:5]%>% # ensure number of columns is correct
  mutate(relief.rank=as.numeric(relief.rank))%>%
  group_by(campaignid,sample) %>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  glimpse()

# CREATE catami_broad------
broad.percent.cover<-habitat%>%
  select(campaignid,sample,starts_with("biota."))%>%
  mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  group_by(campaignid,sample)%>%
  mutate_at(vars(starts_with("biota.")),funs(./Total.Sum*100))%>%
  select(-Total.Sum)%>%
  data.frame()%>%
  glimpse()

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
broad.percent.cover[is.nan(broad.percent.cover)] <- 0 
broad.percent.cover%<>%glimpse()

# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.relief.fov<-relief.mean.and.sd%>%
  full_join(fov.percent.cover,by=c("campaignid","sample"))%>%
  full_join(broad.percent.cover,by=c("campaignid","sample"))%>%
  mutate(id=paste(campaignid,sample,sep="."))%>%
  semi_join(metadata)%>%
  left_join(metadata)%>%
  glimpse()

write.csv(habitat.relief.fov, file=paste(study,"complete.habitat.csv",sep = "."), row.names=FALSE)
write.fst(habitat.relief.fov,"complete.habitat.fst")


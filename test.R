
create.maxn<-function(dat,include.count.files=FALSE){
 maxn <-list.files.GA("_Points.txt")%>% # list all files ending in "_Points.txt"
  purrr::map_df(~read_files_txt(.))%>%
  dplyr::group_by(campaignid,sample,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)
  
if(include.count.files==TRUE){count <-list.files.GA("Count.csv")%>% # list all files ending in "Count.csv"
  purrr::map_df(~read_files_csv(.))

 maxn<-maxn%>% 
  plyr::rbind.fill(count)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=ifelse(maxn%in%c(0,NA),count,maxn))%>% 
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::select(-c(count))%>%
  dplyr::filter(maxn>0)
} 
}

maxn<-create.maxn(include.count.files = TRUE)%>%
  dplyr::inner_join(metadata)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::filter(successful.count=="Yes")%>%
  glimpse()



read_files_csv <- function(flnm) {
  read_csv(flnm,col_types = cols(.default = "c"))%>%
    mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/", extra = "drop", fill = "right")%>%
    clean_names%>%
    plyr::rename(., replace = c(opcode="sample"),warn_missing = FALSE)
}


# try to match length way
create.maxn<-function(dat){

points.files <-list.files.GA("Points.txt") # list all files ending in "Lengths.txt"
points.files$lines<-sapply(points.files,countLines) # Count lines in files (to avoid empty files breaking the script)
  
points<-expand.files(points.files)%>% # remove all empty files
    purrr::map_df(~read_files_txt(.))

count.files <-list.files.GA("Count.csv") # list all files ending in "Lengths.txt"
count.files$lines<-sapply(count.files,countLines) # Count lines in files (to avoid empty files breaking the script)

count<-expand.files(count.files)%>% # remove all empty files
  purrr::map_df(~read_files_csv(.))

# If count is blank but there are points
if (dim(count)[1] == 0 & dim(points)[1] > 0) {
  maxn<-points%>%
    dplyr::group_by(campaignid,sample,frame,family,genus,species)%>%
    dplyr::mutate(number=as.numeric(number))%>%
    dplyr::summarise(maxn=sum(number))%>%
    dplyr::group_by(campaignid,sample,family,genus,species)%>%
    dplyr::slice(which.max(maxn))%>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(maxn))%>%
    dplyr::select(-frame)%>%
    tidyr::replace_na(list(maxn=0))%>%
    dplyr::mutate(maxn=as.numeric(maxn))%>%
    dplyr::filter(maxn>0)
  return(maxn)
  
# if both aren't blank
} else if (dim(count)[1] > 0 & dim(points)[1] > 0) {
  maxn <-points%>%
    dplyr::group_by(campaignid,sample,frame,family,genus,species)%>%
    dplyr::mutate(number=as.numeric(number))%>%
    dplyr::summarise(maxn=sum(number))%>%
    dplyr::group_by(campaignid,sample,family,genus,species)%>%
    dplyr::slice(which.max(maxn))%>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(maxn))%>%
    dplyr::select(-frame)%>%
    tidyr::replace_na(list(maxn=0))%>%
    dplyr::mutate(maxn=as.numeric(maxn))%>%
    dplyr::filter(maxn>0)%>% 
    plyr::rbind.fill(count)%>%
    tidyr::replace_na(list(maxn=0))%>%
    dplyr::mutate(maxn=ifelse(maxn%in%c(0,NA),count,maxn))%>% 
    dplyr::mutate(maxn=as.numeric(maxn))%>%
    dplyr::select(-c(count))%>%
    dplyr::filter(maxn>0)
  return(maxn)
  
  # if only count
} else (dim(count)[1] > 0 & dim(points)[1] == 0)
  maxn <-count%>%
    dplyr::group_by(campaignid,sample,family,genus,species)%>%
    dplyr::rename(maxn=count)%>%
    dplyr::slice(which.max(maxn))%>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(maxn))
  return(maxn)
}


test.empty.count<-create.maxn()

dim(count)[1]

maxn<-points%>%
  dplyr::group_by(campaignid,sample,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)



expand.files<-function(files) {
  as.data.frame(files)%>%
    mutate(campaign=row.names(.))%>%
    filter(lines>1)%>% # filter out all empty text files
    select(campaign)%>%
    as_vector(.)
}










create.length3dpoints<-function(dat){
  
  threedpoints.files <-list.files.GA("3DPoints.txt") # list all files ending in "3DPoints.txt"
  threedpoints.files$lines<-sapply(threedpoints.files,countLines) # Count lines in files (to avoid empty files breaking the script)
  threedpoints<-as.data.frame(threedpoints.files)%>%
    mutate(campaign=row.names(.))%>%
    filter(lines>1)%>% # filter out all empty text files
    select(campaign)%>%
    as_vector(.)%>% # remove all empty files
    purrr::map_df(~read_files_txt(.))
  
  # Combine all downloaded Lengths txt files into one data frame (EM)
  lengths.files <-list.files.GA("Lengths.txt") # list all files ending in "Lengths.txt"
  lengths.files$lines<-sapply(lengths.files,countLines) # Count lines in files (to avoid empty files breaking the script)
  lengths<-as.data.frame(lengths.files)%>%
    mutate(campaign=row.names(.))%>%
    filter(lines>1)%>% # filter out all empty text files
    select(campaign)%>%
    as_vector(.)%>% # remove all empty files
    purrr::map_df(~read_files_txt(.))
  
  # Combine all downloaded generic Length.csv files into one data frame
  length.files <-list.files.GA("Length.csv") # list all files ending in "Lengths.txt"
  length.files$lines<-sapply(length.files,countLines) # Count lines in files (to avoid empty files breaking the script)
  length <-as.data.frame(length.files)%>%
    mutate(campaign=row.names(.))%>%
    filter(lines>1)%>% # filter out all empty text files
    select(campaign)%>%
    as_vector(.)%>% # list all files ending in "Length.csv"
    purrr::map_df(~read_files_csv(.))
  
  # If both Eventmeasure and Generic outputs
  if (dim(lengths)[1] > 0 & dim(length)[1] > 0) {
    length3dpoints<-lengths%>%
      plyr::rbind.fill(threedpoints)%>%
      plyr::rbind.fill(length)%>%
      dplyr::mutate(number=ifelse(number%in%c(0,NA),count,number))%>%
      dplyr::select(-c(count))%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::mutate(number=as.numeric(number))
    return(length3dpoints)
    
    # If only Eventmeausure ouputs
  } else if (dim(lengths)[1] > 0 & dim(length)[2] == 0) {
    length3dpoints<-lengths%>%
      plyr::rbind.fill(threedpoints)%>%
      dplyr::mutate(length=as.numeric(length))%>%
      dplyr::mutate(number=as.numeric(number))
    return(length3dpoints)
    
    # if only Generic outputs
  } else (dim(lengths)[2] == 0 & dim(length)[1] > 0)
  length3dpoints<-length%>%
    dplyr::rename(number=count)%>%
    dplyr::mutate(length=as.numeric(length))%>%
    dplyr::mutate(number=as.numeric(number))
  return(length3dpoints)
}






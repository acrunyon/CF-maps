
########## EXTRACT PROJECTION NAMES
rm(list=ls())
CF.folders <- "E:/RCF_2024/RCF_opened/"

parks <- list.dirs(CF.folders, full.names=FALSE, recursive=FALSE)
park.GCMs <- data.frame()

# CFs1 <- c("Warm Wet", "Hot Dry")
# CFs2 <- c("Warm Dry", "Hot Wet")

for(i in 1:length(parks)){
  
  SiteID <- parks[i]
  session.file <- paste0(CF.folders,parks[i],"/SessionInfo.txt")
  if(!file.exists(session.file)) {print(paste0(SiteID, " failed"));next}
  else{
  ## Extract CFs from SessionInfo
  SessionInfo <- read.table(session.file, sep = "^") #for downloaded centroid CFs
  # SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs
  
  # extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
  GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
  GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
  CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
  CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]
  
  CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
  # CF.GCM <- subset(CF.GCM, CF %in% CFs)
  
  CF.GCM$SiteID <- SiteID
  park.GCMs <- rbind(park.GCMs,CF.GCM)
  }
}

write.csv(park.GCMs,"C:/Users/arunyon/OneDrive - DOI/RCF_projections.csv",row.names = FALSE)



#### EXTRACT FUTURE MEANS

rm(list=ls())
CF.folders <- "E:/RCF_2024/RCF_opened/"

parks <- list.dirs(CF.folders, full.names=FALSE, recursive=FALSE)
future_means_all <- data.frame()

for(i in 1:length(parks)){
  
  SiteID <- parks[i]
  future.means.file <- paste0(CF.folders,parks[i],"/input-data/",parks[i],"_Future_Means.csv")
  if(!file.exists(future.means.file)) {print(paste0(SiteID, " failed"));next}
  else{
    ## Extract future means info
    park.data <- read.csv(future.means.file) 
    park.data$SiteID <- SiteID
    future_means_all <- rbind(future_means_all,park.data)
  }
}

write.csv(future_means_all,"C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/ALL_FUTURE_MEANS.csv",row.names = FALSE)


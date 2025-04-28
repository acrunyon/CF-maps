library(dplyr)

# ID parks that used Tercek or Abatzoglou WB tifs

rm(list=ls())

CF.folders <- "D:/RCF_2024/RCF_opened/"

Tercek <- c("inmcm4","NorESM1-M","MRI-CGCM3","MIROC5","MIROC-ESM-CHEM",
            "IPSL-CM5A-LR","HadGEM2-CC365","GFDL-ESM2G","CanESM2","CSIRO-Mk3-6-0","CNRM-CM5","CCSM4","BNU-ESM")
Tercek.WB <- c(paste0(Tercek,".rcp45"),paste0(Tercek,".rcp85"))

park.list <- c("MORA","APPA", "BLRI","NOCO","LECL","IATR","MISS","CHOH","POHE","OLYM","YELL","SEQU","GRBA","KICA","BICA",
               "YOSE","WICA","NOCA","ZION","GLAC","GRTE","GRSA","ROLA","BIBE","LAKE","GRCA","JOTR","DEVA","SAMO","MOJA",
               "ROMO","MEVE","SAGU","LACH","GRSM")
CFs <- c("Warm Wet","Hot Dry")

list <- data.frame()

for (i in 1:length(park.list)){
SiteID <- park.list[i]
## Extract CFs from SessionInfo
SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
# SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

Tercek <- (CF.GCM$GCM %in% Tercek.WB)

df1 <- data.frame(SiteID=SiteID, T1=Tercek[1],T2=Tercek[2])
list<- rbind(list,df1)
}



park.list <- c("DEWA","SHEN","NERI")
CFs <- c("Warm Wet","Hot Dry")

for (i in 1:length(park.list)){
  SiteID <- park.list[i]
  ## Extract CFs from SessionInfo
  SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
  # SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs
  
  # extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
  GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
  GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
  CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
  CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]
  
  CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
  CF.GCM <- subset(CF.GCM, CF %in% CFs)
  CF1 <- CF.GCM |> filter(CF == CFs[1])
  CF2 <- CF.GCM |> filter(CF == CFs[2])
  
  Tercek <- (CF.GCM$GCM %in% Tercek.WB)
  
  df1 <- data.frame(SiteID=SiteID, T1=Tercek[1],T2=Tercek[2])
  list<- rbind(list,df1)
}

list$SiteID[which()]

list$SiteID[which(list$T1==TRUE & list$T2==TRUE)]

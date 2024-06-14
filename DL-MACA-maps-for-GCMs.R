Lg.parks.W <- c("YELL", "SEQU", "GRBA", "CORO", "KICA", "BICA", "MORA", "YOSE", "WICA", "NOCA", "ZION", "GLAC", "GRTE",
              "GRSA", "ROLA", "BIBE", "LAKE", "GRCA", "JOTR", "OLYM", "NEPE", "DEVA", "SAMO", "MOJA",
              "ROMO", "MEVE", "SAGU", "LACH", "GRSM", "APPA", "BLRI")
Lg.parks.E <- c("DEWA","SHEN","NERI")


Tercek <- c("inmcm4","NorESM1-M","MRI-CGCM3","MIROC5","MIROC-ESM-CHEM",
               "IPSL-CM5A-LR","HadGEM2-CC365","GFDL-ESM2G","CanESM2","CSIRO-Mk3-6-0","CNRM-CM5","CCSM4","BNU-ESM")
Tercek.WB <- c(paste0(Tercek,".rcp45"),paste0(Tercek,".rcp85"))

CF.folders <- "D:/RCF_2024/RCF_opened/"
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/maps"
tifDir <- "D:/MACA_tiffs/"

park.GCMs <- data.frame()

CFs <- c("Warm Wet", "Hot Dry")
cols <- c("#2B83BA", "#D7191C")
for(i in 1:length(Lg.parks.W)){

SiteID <- Lg.parks.W[i]

## Extract CFs from SessionInfo
SessionInfo <- read.table(paste0(CF.folders,Lg.parks.W[i],"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
# SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)

CF.GCM$SiteID <- SiteID
CF.GCM$Tercek <- (CF.GCM$GCM %in% Tercek.WB)
park.GCMs <- rbind(park.GCMs,CF.GCM)
}

un.W <- unique(park.GCMs$GCM)     

park.GCMs <- data.frame()

CFs <- c("Warm Dry", "Hot Wet")
cols <- c("#ABDDA4","#FDAE61")
for(i in 1:length(Lg.parks.E)){
  
  SiteID <- Lg.parks.E[i]
  
  ## Extract CFs from SessionInfo
  SessionInfo <- read.table(paste0(CF.folders,Lg.parks.E[i],"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
  # SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs
  
  # extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
  GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
  GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
  CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
  CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]
  
  CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
  CF.GCM <- subset(CF.GCM, CF %in% CFs)
  
  CF.GCM$SiteID <- SiteID
  CF.GCM$Tercek <- (CF.GCM$GCM %in% Tercek.WB)
  park.GCMs <- rbind(park.GCMs,CF.GCM)
}

un.E <- unique(park.GCMs$GCM) 

uni <- c(un.E,un.W)
uni <- sort(unique(uni))

## Download from MACA server
period <- "20402069"

downloads <- data.frame(projection=uni)
downloads$GCM <- sub("\\..*", "", downloads$projection) #everything before period
downloads$RCP <- sub('.*\\.', '', downloads$projection) #everything after period

downloads$tasmean_ANN <- paste0("https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/macav2metdata_tasmean_ANN_",
                                period,"_",downloads$RCP,"_vs_19712000_",downloads$GCM,".tif")

downloads$pr_ANN <- paste0("https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/macav2metdata_pr_ANN_",
                                period,"_",downloads$RCP,"_vs_19712000_",downloads$GCM,".tif")
downloads$deficit_ANN <- paste0("https://climate.northwestknowledge.net/MWBM_MACAV2METDATA/TIF/MWBM_macav2metdata_deficit_ANN_",
                                period,"_",downloads$RCP,"_vs_19712000_",downloads$GCM,".tif")

# wb.maca.dls <- subset(downloads, !(projection %in% Tercek.WB))
# 
# wb.maca.dls$deficit_ANN <- paste0("https://climate.northwestknowledge.net/MWBM_MACAV2METDATA/TIF/MWBM_macav2metdata_deficit_ANN_",
#                                 period,"_",wb.maca.dls$RCP,"_vs_19712000_",wb.maca.dls$GCM,".tif")

for (i in 1:nrow(downloads)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("macav2metdata_tasmean_ANN_",period,"_",downloads$RCP[i],"_vs_19712000_",downloads$GCM[i],".tif")
  download.file(downloads$tasmean_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

for (i in 1:nrow(downloads)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("macav2metdata_pr_ANN_",period,"_",downloads$RCP[i],"_vs_19712000_",downloads$GCM[i],".tif")
  download.file(downloads$pr_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

for (i in 1:nrow(downloads)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("MWBM_macav2metdata_deficit_ANN_",period,"_",downloads$RCP[i],"_vs_19712000_",downloads$GCM[i],".tif")
  download.file(downloads$deficit_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

# for (i in 1:nrow(wb.maca.dls)){ #Run as loop to avoid timeout - which happens when calling too many units
#   # for (i in 351:length(links)-1){
#   file.name <- paste0("MWBM_macav2metdata_deficit_ANN_",period,"_",wb.maca.dls$RCP[i],"_vs_19712000_",wb.maca.dls$GCM[i],".tif")
#   download.file(wb.maca.dls$deficit_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
# }

# Tercek wb downloads
wb.tercek.dls <- subset(downloads, projection %in% Tercek.WB)

wb.tercek.dls$deficit_ANN <- paste0("http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/Deficit/",wb.tercek.dls$RCP,
                                    "/V_1_5_annual_",wb.tercek.dls$GCM,"_",wb.tercek.dls$RCP,"_Deficit_2040_2069_annual_means_cropped_units_mm.tif")

for (i in 1:nrow(wb.tercek.dls)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("V_1_5_annual_",wb.tercek.dls$GCM[i],"_",wb.tercek.dls$RCP[i],"_Deficit_2040_2069_annual_means_cropped_units_mm.tif")
  download.file(wb.tercek.dls$deficit_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

write.csv(downloads,paste0(tifDir,"downloads.for.maps.csv"),row.names = FALSE)

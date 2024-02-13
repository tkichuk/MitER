
## Install necessary packages
install.packages("rio")
install.packages("stringi")
install.packages ("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggbeeswarm")
install.packages("EnvStats")
install.packages("ggpubr")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("htmltools")
install.packages("treemapify")

## Load necessary packages
library(plyr)
library(hrbrthemes)
library(tidyr)
library(viridis)
library (stringi)
library(stringr)
library(rio) 
library(readxl)
library (tidyr)
library (dplyr)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(EnvStats)
library(ggpubr)
library(treemapify)



## Save R script to the folder on your desktop that contains your data output folder
DataFolder <- dirname(rstudioapi::getSourceEditorContext()$path)
SummaryPath <- file.path(DataFolder, "MammalianData")
setwd(SummaryPath)
getwd()



## Read csv outputs from Python-Blender script  
OverlapFilenames <- list.files(pattern="Overlap.*csv")
CellStatsFilenames <- list.files(pattern="CellStats.*csv")
MitoComponentFilenames <- list.files(pattern="MitoComponents.*csv")
MitoComponentFilenames
MitochondrialDistributionFilenames <- list.files(pattern="-MitochondrialDistribution.*csv")
ContactDistributionFilenames <- list.files(pattern="-ContactDistribution.*csv")
MitochondrialAreaDistributionFilenames <- list.files(pattern="-MitochondrialAreaDistribution.*csv")


## Load all files into R

for(i in CellStatsFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",4)),
                         sep = "\t"))
  }
}

for(i in ContactDistributionFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",4)),
                         sep = "\t"))
  }
}

EmptyFiles <- "List"
MitoComponentFilenames
for(i in MitoComponentFilenames){
  if (file.size(i) > 0) {
    print (i)
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",6)),
                         sep = "\t"))
  }
  else {
    print (i)
    EmptyFiles <- c(EmptyFiles, i)
  }
}

EmptyOverlapFiles <- "List"

for(i in OverlapFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",6)),
                         sep = "\t"))
  }
  else {
    print (i)
    EmptyOverlapFiles <- c(EmptyOverlapFiles, i)
  }
}

EmptyOverlapFiles

for(i in MitochondrialDistributionFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",6)),
                         sep = "\t"))
  }
  else {
    print (i)
  }
}

for(i in MitochondrialAreaDistributionFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",6)),
                         sep = "\t"))
  }
  else {
    print (i)
  }
}



## Create a list of all the files that were loaded into R
AllDataFrames <- sapply(sapply(ls(), get), is.data.frame)
names(AllDataFrames)[(AllDataFrames==TRUE)] 
CellStatsFiles<-grep("CellStats.csv",names(.GlobalEnv),value=TRUE)
CellStatsFiles
CellStats <- data.frame()


###For troubleshooting
#i= CellStatsFiles [1]
#i

## Preprocess CellStats files and combine into one dataframe
for (i in CellStatsFiles) {
  DF <- get(i)
  print(i)
  if (nrow(DF) %in% c(39,31, 23, 15)) {  # Corrected the condition
    
    RowOne <- colnames(DF[1])
    ColTwo <- rbind(RowOne, DF[1])
    DF <- as.data.frame(ColTwo[, 1])
    DFinal <- data.frame(matrix(DF[, 1], ncol = 8, byrow = TRUE))
    ColNames <- c("Organelle", "SurfaceArea", "Volume", "BBHeight", "BBLength", "BBWidth", "CenterOfGravity", "BBVerts")
    colnames(DFinal) <- ColNames
    RowNames <- c(DFinal$Organelle)
    
    COGs <- as.character(DFinal$CenterOfGravity)
    COGs <- strsplit(COGs, ",")
    COGs <- lapply(X = COGs, FUN = function(t) gsub(pattern = "[^0-9.-]", replacement = "", x = t))
    COGs <- as.numeric(unlist(COGs))
    COGsDF <- data.frame(matrix(COGs, ncol = 3, byrow = TRUE))
    row.names(COGsDF) <- RowNames
    CellCOG <- COGsDF[rownames(COGsDF) == "Cell", ]
    COGsDF <- cbind(COGsDF, CellCOG)
    DFinal$COGDifference <- apply(COGsDF, 1, function(x) dist(matrix(x, nrow = 2, byrow = TRUE)))
    DFinal$id <- 1
    DFinal <- reshape(DFinal, idvar = "id", timevar = "Organelle", direction = "wide")[-1]
    
    File <- gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = as.character(i))  # Ensure i is converted to character
    DFinal <- cbind("File" = File, DFinal)  # Use "File" as column name
    CellStats <- rbind(CellStats, DFinal)
  }
}




## Collate distribution files
## Apply cutoffs to remove outliers
MitochondrialDistributionFiles<-grep("-MitochondrialDistribution.csv",names(.GlobalEnv),value=TRUE)
ContactDistributionFiles<-grep("-ContactDistribution.csv",names(.GlobalEnv),value=TRUE)
MitochondrialAreaDistributionFiles<-grep("-MitochondrialAreaDistribution.csv",names(.GlobalEnv),value=TRUE)

AllMetrics <-  data.frame(matrix(ncol = 16, nrow = 0))
MasterMetrics <- data.frame(matrix(ncol = 16, nrow = 0))

##Based on the theoretical limit of resolution in Z for the W1 SoRa microscope d = 2λ/(NA)2##
CutOff= 0.009

MasterMito <- data.frame(matrix(ncol = 16, nrow = 0))
ContactCounts <- data.frame(matrix(ncol = 22, nrow = 0))

###For troubleshooting
#i <- MitochondrialDistributionFiles [1]
#i

###GETS ALL DISTRIBUTION DATA INCLUDING CONTACT DISTRIBUTION###
## Iterate through all distribution files
MitochondrialDistributionFiles

for(i in MitochondrialDistributionFiles) {
  j = sapply(X =i, FUN = function(t) gsub(pattern = "-MitochondrialDistribution.csv", replacement = "-MitochondrialAreaDistribution.csv", x = t))
  ContactDistFile = sapply(X =i, FUN = function(t) gsub(pattern = "-MitochondrialDistribution.csv", replacement = "-ContactDistribution.csv", x = t))
  
  # Get distribution files for mitochondria
  DF <- get(i)
  DF [1]
  
  # Get distribution files for corresponding contacts to current mitochondria
  DF2<- get(j)
  DF2 [1]
  
  ###FOR THREE OR FIVE STRUCTURES IF THERE IS CONTACT DISTRIBUTION DATA###
  if (nrow (DF) > 0 & nrow (DF2) > 0) {  # Make sure file is not empty
    
    # ~ Formatting ~
    RowOne <- colnames(DF2 [1])
    RowOne
    ColTwo <- rbind (RowOne,DF2[1])
    DF2 <- as.data.frame (ColTwo[,1])
    DF2
    DFinal2 <- data.frame(matrix(DF2[,1], ncol = 6, byrow = TRUE))
    ColNames <- c ("SectionName", "CubeSA", "DifferenceSA", "CubeVolume","DifferenceVolume", "SectionVerts")
    colnames(DFinal2) <- ColNames
    
    
    RowOne <- colnames(DF[1])
    ColTwo <- rbind (RowOne,DF[1])
    DF <- as.data.frame (ColTwo[,1])
    DFinal <- data.frame(matrix(DF[,1], ncol = 4, byrow = TRUE))
    ColNames <- c ("SectionName", "SectionSA", "SectionVol", "SectionVerts")
    colnames(DFinal) <- ColNames
    
    
    # For each of 64 subcube sections, collate mitochondria and contact distribution data
    DFinal <- merge (DFinal, DFinal2, by = c("SectionName", "SectionVerts"))
  
    
    # Compute mitochondrial surface area in each subcube section
    DFinal$SectionMitoSA <- (as.numeric(DFinal$SectionSA) + as.numeric(DFinal$DifferenceSA) - as.numeric(DFinal$CubeSA))/2
    
    
    # Extract the value from the desired column at the midpoint
    TotalMitoSA <- DFinal$SectionMitoSA[round(nrow(DFinal) / 2)]
    
    # Normalize mitochondrial surface area in each subcube section by total mitochondrial surface area in cell
    DFinal$FractionSectionMitoSA <- round((DFinal$SectionMitoSA/TotalMitoSA ), digits=3)
    
    File <- gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = as.character(i)) 
    
    DFinal <- cbind (File, DFinal)
    
    DFinal <- merge (DFinal, CellStats, by = "File")
    
    # Get file storing the distribution of the cell
    DFII <- get(ContactDistFile)
    
    
    # Check that file is not empty and format data frame
    if (nrow (DFII) > 0){
        RowOne <- colnames(DFII[1])
        ColTwo <- rbind (RowOne,DFII[1])
        DFII <- as.data.frame (ColTwo[,1])
        DFinalII <- data.frame(matrix(DFII[,1], ncol = 4, byrow = TRUE))
        ColNames <- c ("SectionName", "ContactSectionSA", "ContactSectionVol", "SectionVerts")
        colnames(DFinalII) <- ColNames
    }
    
    # Merge cell distribution data frame with mitochondrial and contact distribution data by shared subcube vertices
    DFinal <- merge (DFinal, DFinalII, by = c("SectionName", "SectionVerts"))
    DFinal$SectionFraction <- as.numeric(gsub("[A-Za-z]", "", DFinal$SectionName))
    DFinal$SectionCellRadius <- (((as.numeric(DFinal$BBHeight.Cell)*DFinal$SectionFraction)+as.numeric(DFinal$BBLength.Cell)*DFinal$SectionFraction+ as.numeric(DFinal$BBWidth.Cell)*DFinal$SectionFraction)/3)/2
    DFinal$SectionCellRadius
    
    # Compute total volume of mitochondria in cell
    TotalMitoVol <-  DFinal[which(DFinal$SectionName == "CellCopyInside1.0"),which(colnames(DFinal)=="SectionVol")]
    TotalMitoVol
    
    

    # Normalize mitochondrial surface area in each subcube section by total mitochondrial surface area in cell
    DFinal$FractionSectionMitoVol <- round((as.numeric(DFinal$SectionVol)/as.numeric(TotalMitoVol)), digits=3)
    
    DFinal <- subset(DFinal, select = !names(DFinal) %in% names(CellStats))
    DFinal <- cbind (File, DFinal)
    
    
    AllMetrics <- rbind (AllMetrics, DFinal)
  }
  
  
  FileName<-gsub("MitochondrialDistribution.csv", "", i)
  OverlapFile <- paste0(FileName, "Overlap.csv") 
  FileName
  paste0 (FileName, "Overlap.csv")
  grep(paste0 (FileName, "Overlap.csv"),names(.GlobalEnv),value=TRUE)
  if (OverlapFile %in% EmptyOverlapFiles == "FALSE"){
  
    OverlapFile <-get(grep(paste0 (FileName, "Overlap.csv"),names(.GlobalEnv),value=TRUE))
    RowOne <- colnames(OverlapFile[1])
    RowOne
    ColTwo <- rbind (RowOne,OverlapFile[1])
    OverlapFile <- as.data.frame (ColTwo[,1])
    OverlapFileFinal <- data.frame(matrix(OverlapFile[,1], ncol = 3, byrow = TRUE))
    
    ColNames <- c ("SurfaceArea", "Volume", "BBVerts")
    colnames(OverlapFileFinal) <- ColNames
    
    str <- OverlapFileFinal [1]
    str <- lapply(X = str, FUN = function(t) gsub(pattern = ".*:", replacement = "", x = t))
    str <- lapply(X = str, FUN = function(t) gsub(pattern = "^\\D+", replacement = "\\1", x = t))
    str <- lapply(X =str, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    str <-lapply(X =str, FUN = function(t) gsub(pattern = "e.", replacement = "e-", x = t))
    OverlapFileFinal [1] <-as.numeric (unlist(str))
    
    File <- gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = as.character(i)) 
    OverlapFileFinal <- cbind (File, OverlapFileFinal)
    
    OverlapDeetsDF <-data.frame(matrix(ncol = 3, nrow = 0))
    Metrics <-data.frame(matrix(ncol = 6, nrow = 0))
    
    for (n in unique(DFinal$SectionVerts)){
      print (n)
      #n <-DFinal$SectionVerts [1]
      Verts <- n
      Verts <- strsplit (Verts, ",")
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = " ", replacement = "", x = t))
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "\\[|\\]", replacement = "", x = t))
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "[()]", replacement = "", x = t))
      Verts <- as.numeric(unlist(Verts))
      VertsDF <- as.data.frame (Verts)
      VertsDF <-data.frame(matrix(VertsDF[,1], ncol = 3, byrow = TRUE))
      colnames(VertsDF) <- c("X", "Y", "Z")
      MaxX = max (VertsDF$X)
      MinX = min (VertsDF$X)
      MaxY = max (VertsDF$Y)
      MinY = min (VertsDF$Y)
      MaxZ = max (VertsDF$Z)
      MinZ = min (VertsDF$Z)
     
      for (i in OverlapFileFinal$BBVerts ){
        Verts <- i
        Verts
        Verts <- strsplit (Verts, ",")
        Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = " ", replacement = "", x = t))
        Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "\\[|\\]", replacement = "", x = t))
        Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "[()]", replacement = "", x = t))
        Verts <- as.numeric(unlist(Verts))
        VertsDF <- as.data.frame (Verts)
        VertsDF <-data.frame(matrix(VertsDF[,1], ncol = 3, byrow = TRUE))
        colnames(VertsDF) <- c("A", "B", "C")
        MaxA = max (VertsDF$A)
        MinA = min (VertsDF$A)
        MaxB = max (VertsDF$B)
        MinB = min (VertsDF$B)
        MaxC = max (VertsDF$C)
        MinC = min (VertsDF$C)
        BBOverlapVolume <- max(min(MaxA,MaxX)-max(MinA,MinX),0) * max(min(MaxB,MaxY)-max(MinB,MinY),0)* max(min(MaxC,MaxZ)-max(MinC,MinZ),0)
        BBVolume <- abs(MaxA-MinA) * abs(MaxB-MinB) * abs(MaxC-MinC)
        FractionBBOverlap <- BBOverlapVolume/BBVolume
        OverlapDeet<- as.data.frame (t(c(FractionBBOverlap, i, n)))
        colnames(OverlapDeet) <- c("FractionBBOverlap", "BBVerts", "SectionVerts")
        OverlapDeetsDF <-rbind(OverlapDeetsDF, OverlapDeet)
        }
      }
      colnames(OverlapDeetsDF) <- c("FractionBBOverlap", "BBVerts", "SectionVerts")
      DFinalWithContact <- merge (DFinal, OverlapDeetsDF, by= "SectionVerts")
      DFinalWithContact <- merge (DFinalWithContact,OverlapFileFinal [-1], by = "BBVerts")
      
      DFinalWithContact$Volume
      #Remove NaN's in case there is an edge cast with 0 computed volume
      DFinalWithContact<- DFinalWithContact[!grepl("NaN", DFinalWithContact$FractionBBOverlap),]
      DFinalWithContact<-DFinalWithContact[grepl("Inside", DFinalWithContact$SectionName),]
      DFinalWithContactFiltered <- DFinalWithContact[which(DFinalWithContact$Volume > CutOff),]
      DFinalWithContact$TrueSA <-DFinalWithContact$SurfaceArea * as.numeric(DFinalWithContact$FractionBBOverlap)
      DFinalWithContactFiltered$TrueSA <-DFinalWithContactFiltered$SurfaceArea * as.numeric(DFinalWithContactFiltered$FractionBBOverlap)
     
    for (Section in unique (DFinalWithContact$SectionName) ){
      print (Section)
      ContactCount <- sum (as.numeric(DFinalWithContact[DFinalWithContact$SectionName == Section, "FractionBBOverlap"]))
      FilteredContactCount <- sum (as.numeric(DFinalWithContactFiltered[DFinalWithContactFiltered$SectionName == Section, "FractionBBOverlap"]))
      ContactSA <- sum (as.numeric(DFinalWithContact[DFinalWithContact$SectionName == Section, "TrueSA"]))
      FilteredContactSA <- sum (as.numeric(DFinalWithContactFiltered[DFinalWithContactFiltered$SectionName == Section, "TrueSA"]))
      ContactCountDF <- as.data.frame (t(c(File, Section, ContactCount, ContactSA, FilteredContactCount, FilteredContactSA)))
      colnames(ContactCountDF) <- c ("File", "SectionName", "ContactCount", "ContactSA", "FilteredContactCount", "FilteredContactSA")
      ContactCounts <- rbind (ContactCounts, ContactCountDF)
    }
    MasterMetrics <- merge (AllMetrics, ContactCounts, by = c("File", "SectionName"))
  }
  if (OverlapFile %in% EmptyOverlapFiles == "TRUE") {
  MasterMetrics <- rbind (MasterMetrics, AllMetrics)
  }
}  

#Peripheral contact surface area normalized to peripheral mitochondrial surface area
MasterMetrics$NormContactDist <- (as.numeric(MasterMetrics$ContactSA)/as.numeric(MasterMetrics$SectionMitoSA))
MasterMetrics$NormContactDistFiltered<- (as.numeric(MasterMetrics$FilteredContactSA)/as.numeric(MasterMetrics$SectionMitoSA))


MasterMetrics$ID <- MasterMetrics$File

MasterMetrics <- reshape(MasterMetrics, idvar = "ID", timevar = "SectionName", direction = "wide")[-1]


# Set first column name to "File"
colnames(MasterMetrics)[1] <- "File"

# Remove duplicate columns containing the word "File"
MasterMetrics<- MasterMetrics[, -c(grep("File.CellCopy", colnames(MasterMetrics)))]

# Adding distribution metrics into CellStats dataframe
CellStats <- merge (CellStats, MasterMetrics, by= "File")


# Extract Mitochondria Files
MitoStatsFiles<-grep("MitoComponents.csv",names(.GlobalEnv),value=TRUE)
FullMitoStats <- data.frame(matrix(ncol = 5, nrow = 0))

#i = MitoStatsFiles [1]
# Iterate through each Mitochondria File
for(i in MitoStatsFiles){
  #Get file and format strings
  #Getting mitochondrial data from the file#
  File<- gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = as.character(i)) 
  DF <- get(i)
  DF <- DF [1]
  RowOne <- colnames(DF [1])
  RowOne <- gsub("..", ",", RowOne, fixed = TRUE)
  RowOne <- gsub("e.", "e-", RowOne, fixed = TRUE)
  RowOne <- strsplit(RowOne, ",")
  RowOne <- lapply(X =RowOne, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
  RowOne <- lapply(X = RowOne, FUN = function(t) gsub(pattern = "^\\D+", replacement = "\\1", x = t))
  RowOne <- as.numeric(unlist(RowOne))
  MitoStats <- rbind (as.data.frame(DF), as.data.frame(RowOne))
  colnames(MitoStats) <- c("Volume")
  MitoStats$Volume <- as.numeric (MitoStats$Volume)
  
  #Getting file name for mitochondrial data#
  MitoCutOff= 0.009
  TotalMitoComponents = 0
  MitoCutOffComponents = 0
  TotalMitoVolume = 0
  MitoCutOffVolume = 0
  
  # Iterate through each row of Mitochondria File
  for (i in MitoStats$Volume) {
    TotalMitoComponents = as.numeric (TotalMitoComponents + 1)
    TotalMitoVolume = as.numeric (TotalMitoVolume + as.numeric (i))
    
    # Applying Cutoffs
    if (i >= MitoCutOff){
      MitoCutOffComponents = as.numeric (MitoCutOffComponents + 1)
      MitoCutOffVolume = as.numeric (MitoCutOffVolume + as.numeric (i))
    }
  }
  # Adding to stats
  Stats <- c(File, TotalMitoComponents,TotalMitoVolume, MitoCutOffComponents, MitoCutOffVolume)
  FullMitoStats = rbind (FullMitoStats,Stats)
  colnames(FullMitoStats) <- c("File", "TotalMitoComponents","TotalMitoVolume", "MitoCutOffComponents", "MitoCutOffVolume")
}

# Merging all stats into one dataframe
SummaryFile <- merge(CellStats,FullMitoStats,by="File")

SummaryFile$ScaledCellVolume <- SummaryFile$CubeVolume.CellCopyInside1.0
SummaryFile$ScaledCellSA <- SummaryFile$CubeSA.CellCopyInside1.0

SummaryFile$MitoSurfaceAreaToVolume <- as.numeric(SummaryFile$SurfaceArea.Mitochondria)/as.numeric(SummaryFile$Volume.Mitochondria)

# Dividing surface area by two to prevent double counting of Mito-ER contact area
SummaryFile$HalfOverlapSA <- as.numeric(SummaryFile$ContactSA.CellCopyInside1.0)/2

# Fraction of mitochondria surface area that is in contact with ER
SummaryFile$MitoOverlap <- (SummaryFile$HalfOverlapSA/as.numeric(SummaryFile$SurfaceArea.Mitochondria))

# Fraction of ER surface area that is in contact with Mitochondria
SummaryFile$EROverlap <- (SummaryFile$HalfOverlapSA/as.numeric(SummaryFile$SurfaceArea.ER)) 

# ER volume normalized by cell volume (scaling factor applied to cell volume if computed from ER)
SummaryFile$NormalizedERVol <-(as.numeric(SummaryFile$Volume.ER)/as.numeric(SummaryFile$ScaledCellVolume))

SummaryFile$NormalizedContactCount <-as.numeric(SummaryFile$FilteredContactCount.CellCopyInside1.0)/as.numeric(SummaryFile$SurfaceArea.Mitochondria)



# Mitochondrial volume normalized by cell volume
SummaryFile$NormalizedMitoVol <-(as.numeric(SummaryFile$Volume.Mitochondria)/as.numeric(SummaryFile$ScaledCellVolume))
SummaryFile$NormalizedMitoVol <-(as.numeric(SummaryFile$SubcellularStructure)/as.numeric(SummaryFile$ScaledCellVolume))


SummaryFile$MeanCellRadius <-((as.numeric(SummaryFile$BBHeight.Cell)+as.numeric(SummaryFile$BBLength.Cell)+ as.numeric(SummaryFile$BBWidth.Cell))/3)/2

SummaryFile$AsymmetryMetric <- (as.numeric(SummaryFile$COGDifference.Mitochondria)/ as.numeric(SummaryFile$MeanCellRadius))
SummaryFile$AsymmetryMetric <- (as.numeric(SummaryFile$COGDifference.ER)/ as.numeric(SummaryFile$MeanCellRadius))

SectionCellRadius<- SummaryFile[grepl("SectionCellRadius.CellCopyInside", colnames(SummaryFile))]
FilteredContactSA<- SummaryFile[grepl("FilteredContactSA.CellCopyInside", colnames(SummaryFile))]
ColNumber <- length(SectionCellRadius)
TotalDistributionDF <- data.frame(matrix(0, ncol = ColNumber, nrow = 0))

#ForTroubleShooting
for (file in SummaryFile$File){
  print (file)
  DFfile <- SummaryFile[SummaryFile$File==file,]
  DFfile$SectionCellRadius.CellCopyInside1.0
  DFfile$ContactSA.CellCopyInside1.0
  ContactSA
  SectionCellRadius <- unlist(as.list (DFfile[grepl("SectionCellRadius.CellCopyInside", colnames(DFfile))]))
  SectionCellFraction <- unlist(as.list (DFfile[grepl("SectionFraction.CellCopyInside", colnames(DFfile))]))
  ContactSA<- DFfile[grepl("ContactSA.CellCopyInside", colnames(DFfile))]
  ContactSA<-unlist(as.list (ContactSA[, !grepl("FilteredContactSA", colnames(ContactSA))]))
  FractionContactSA <- as.numeric(ContactSA)/as.numeric(DFfile$ContactSA.CellCopyInside1.0)
  FractionContactSA
  FractionOutsideContactSA<-1-FractionContactSA
  FractionMitochondrialVolume<- unlist(as.list(DFfile[grepl("FractionSectionMitoVol.CellCopyInside", colnames(DFfile))]))
  FractionOutsideMitochondrialVolume<- 1- FractionMitochondrialVolume
  
  
  if (all(FractionMitochondrialVolume <= 1)){
  DistributionDF <- as.data.frame(file)
  DistributionDF <- cbind(DistributionDF, as.numeric(SectionCellFraction), as.numeric(FractionContactSA),as.numeric(FractionOutsideContactSA),as.numeric(FractionMitochondrialVolume),as.numeric(FractionOutsideMitochondrialVolume))
  colnames (DistributionDF) <- c ("File", "SectionCellFraction", "FractionContactSA", "FractionOutsideContactSA","FractionMitochondrialVolume","FractionOutsideMitochondrialVolume")
  TotalDistributionDF <- rbind (TotalDistributionDF, DistributionDF)
  }
  else{
    print (file)
  }
}


SimpleSummary<- TotalDistributionDF

#EXPORT CSV DATA FILES
write.csv(SimpleSummary,paste0(getwd(),"CellDistributionSummary.csv"), row.names = FALSE)
write.csv(SummaryFile,paste0(getwd(),"CellStatsSummary.csv"), row.names = FALSE)





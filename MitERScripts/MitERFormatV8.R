
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
SummaryPath <- file.path(DataFolder, "FermRespData")
setwd(SummaryPath)
getwd()

## Read csv outputs from Python-Blender script  
OverlapFilenames <- list.files(pattern="Overlap.*csv")
DistributionFilenames <- list.files(pattern="-Distribution.*csv")
CellDistributionFilenames <- list.files(pattern="-CellDistribution.*csv")
ContactDistributionFilenames <- list.files(pattern="-ContactDistribution.*csv")
MitoComponentFilenames <- list.files(pattern="MitoComponents.*csv")
CellStatsFilenames <- list.files(pattern="CellStats.*csv")


## Load all files into R

for(i in CellStatsFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",4)),
                         sep = "\t"))
  }
}

for(i in CellDistributionFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",4)),
                         sep = "\t"))
  }
}

for(i in MitoComponentFilenames){
  if (!file.size(i) == 0) {
    filepath <- file.path(SummaryPath,paste(i))
    assign(i, read.delim(filepath,
                         colClasses=c("character","factor",rep("numeric",6)),
                         sep = "\t"))
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

for(i in DistributionFilenames){
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

for(i in ContactDistributionFilenames){
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
CellStats <- data.frame ()


## Preprocess CellStats files and combine into one dataframe
for(i in CellStatsFiles){
  DF <- get(i)
  print (i)
  if (nrow (DF) == 34 | 20 | 13){
    RowOne <- colnames(DF[1])
    ColTwo <- rbind (RowOne,DF[1])
    DF <- as.data.frame (ColTwo[,1])
    DFinal <- data.frame(matrix(DF[,1], ncol = 7, byrow = TRUE))
    ColNames <- c ("SurfaceArea", "Volume", "BBHeight", "BBLength", "BBWidth", "CenterOfGravity", "BBVerts")
    colnames(DFinal) <- ColNames
    #Here the rows should correspond to the number of structures
    
    
    str <- as.character (DFinal [1])
    RowOneName <- str_replace(str, "X.bpy.data.objects..", "")
    RowOneName <- gsub("\\..*","",RowOneName)
    RowOneName <- strsplit(RowOneName , "\"")
    RowNames <- strsplit(str , "'") 
    RowNames <- data.frame (RowNames)
    RowOneName <- data.frame (RowOneName)
    toDelete <- seq(2, nrow(RowNames), 2)
    RowNames <- RowNames[toDelete ,]
    as.character (RowOneName [2,1])
    RowNames <- c(as.character (RowOneName [2,1]), RowNames)
    #Remove everything after period
    RowNames <- gsub("\\..*","",RowNames)
    
    ## Extract vertices for centers of gravity
    ## And compute difference between center of gravity of cell and center of gravity of cell components
    COGs <- as.character (DFinal [6])
    COGs <- strsplit(COGs , ",") 
    COGs <- lapply(X =COGs, FUN = function(t) gsub(pattern = "[^0-9.-]", replacement = "", x=t))
    COGs <- as.numeric(unlist(COGs))
    COGsDF <- data.frame(matrix(COGs, ncol = 3, byrow = TRUE))
    rownames(COGsDF) <- RowNames
    CellCOG <- COGsDF[rownames(COGsDF)=="Cell",]
    COGsDF <-cbind(COGsDF, CellCOG)
    DFinal$COGDifference <-apply(COGsDF, 1, function(x) dist(matrix(x, nrow = 2, byrow = TRUE)))
    
    #Get digits after period
    str <- gsub("[^[:digit:]., ]", "", DFinal [1])
    #Get list of strings split by comma
    str <- strsplit(str, ",")
    
    str <- lapply(X = str, FUN = function(t) gsub(pattern = "^\\D+", replacement = "", x = t))
    str <- lapply(X = str, FUN = function(t) gsub(pattern = ".+? ", replacement = "", x = t))
    str <- lapply(X = str, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    DFinal [1] <- as.numeric (unlist(str))
    str <- lapply(X =i, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    str <- stri_split_fixed(str,"-",n = 3)
    str <- data.frame (str)
    File <- as.character (str [3,1])
    DFinal$id <- 1
    DFinal <- cbind (RowNames, DFinal)
    DFinal <- reshape(DFinal, idvar="id", timevar="RowNames", direction="wide")[-1]
    #Condition <- gsub("-.*","",File)
    #Condition <- gsub("^\\d+|\\d+$", "", Condition)
    #SubStr <- paste(Condition, "-", sep="")
    #CellID <- gsub(SubStr,"",File)
    #print (CellID)
    DFinal <- cbind (File, DFinal)
    CellStats <- rbind (CellStats, DFinal)
  }
}



## Collate distribution files
## Apply cutoffs to remove outliers
DistributionFiles<-grep("-Distribution.csv",names(.GlobalEnv),value=TRUE)
CellDistributionFiles<-grep("-CellDistribution.csv",names(.GlobalEnv),value=TRUE)
AllMetrics <-  data.frame(matrix(ncol = 0, nrow = 17))
MasterMetrics <- data.frame(matrix(ncol = 24, nrow = 0))

##Based on the theoretical limit of resolution in Z for the W1 SoRa microscope d = 2λ/(NA)2##
FirstCutOff= .15
SecondCutOff= .14
ThirdCutOff= .11


#i <- DistributionFiles [1]

###GETS ALL DISTRIBUTION DATA INCLUDING CONTACT DISTRIBUTION###
## Iterate through all distribution files

for(i in DistributionFiles){
  j = sapply(X =i, FUN = function(t) gsub(pattern = "-Distribution.csv", replacement = "-ContactDistribution.csv", x = t))
  CellDistFile = sapply(X =i, FUN = function(t) gsub(pattern = "-Distribution.csv", replacement = "-CellDistribution.csv", x = t))
  
  # Get distribution files for mitochondria
  DF <- get(i)
  
  # Get distribution files for corresponding contacts to current mitochondria
  DF2<- get(j)
  
  ###FOR THREE OR FIVE STRUCTURES IF THERE IS CONTACT DISTRIBUTION DATA###
  if (nrow (DF) > 0 & nrow (DF2) > 0) {  # Make sure file is not empty
    
    # ~ Formatting ~
    RowOne <- colnames(DF2 [1])
    ColTwo <- rbind (RowOne,DF2[1])
    DF2 <- as.data.frame (ColTwo[,1])
    DFinal2 <- data.frame(matrix(DF2[,1], ncol = 6, byrow = TRUE))
    ColNames <- c ("SectionName", "CubeSA", "DifferenceSA", "CubeVolume","DifferenceVolume", "SectionVerts")
    colnames(DFinal2) <- ColNames
    DFinal2$CubeSA <- gsub("[^[:digit:]. ]", "", DFinal2$CubeSA)
    DFinal2$DifferenceSA <- gsub("[^[:digit:]. ]", "", DFinal2$DifferenceSA)

    
    RowOne <- colnames(DF[1])
    ColTwo <- rbind (RowOne,DF[1])
    DF <- as.data.frame (ColTwo[,1])
    DFinal <- data.frame(matrix(DF[,1], ncol = 4, byrow = TRUE))
    ColNames <- c ("SectionName", "SectionSA", "SectionVol", "SectionVerts")
    colnames(DFinal) <- ColNames
    DFinal$SectionSA <- gsub("[^[:digit:]. ]", "", DFinal$SectionSA)
    
    # For each of 64 subcube sections, collate mitochondria and contact distribution data
    DFinal <- merge (DFinal, DFinal2, by = "SectionVerts")
    
    # Compute mitochondrial surface area in each subcube section
    DFinal$SectionMitoSA <- (as.numeric(DFinal$SectionSA) + as.numeric(DFinal$DifferenceSA) - as.numeric(DFinal$CubeSA))/2
    
    #Check Surface Area of Mitochondria
    sum(as.numeric (DFinal$SectionMitoSA))
    
    #Check Volume of Mitochondria
    sum(as.numeric(DFinal$SectionVol))
    
    # Normalize mitochondrial surface area in each subcube section by total mitochondrial surface area in cell
    DFinal$PercentSectionMitoSA <- (DFinal$SectionMitoSA/sum(DFinal$SectionMitoSA))*100
    
    # Extract experimental parameters from filename
    str <- lapply(X =i, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    
    
    # Find corresponding cell ditribution file
    #CellDistFile <- paste0(str, "-All-CellDistribution.csv")
    str <- stri_split_fixed(str,"-",n = 3)
    str <- data.frame (str)
    File <- as.character (str [3,1])
    Condition <- gsub("-.*","",File)
    Condition <- gsub("^\\d+|\\d+$", "", Condition)
    SubStr <- paste(Condition, "-", sep="")
    CellID <- gsub(SubStr,"",File)
    
    # Add extracted parameters to data frame for each subcube section
    Subset <-CellStats[CellStats$File == File,]
    DFinal <- cbind (File, Condition, CellID, DFinal, Subset$BBVerts.Cell)
    
    
    # Get file storing the distribution of the cell
    DFII <- get(CellDistFile)
    

    # Check that file is not empty and format data frame
    if (nrow (DFII) > 0){
      RowOne <- colnames(DFII[1])
      ColTwo <- rbind (RowOne,DFII[1])
      DFII <- as.data.frame (ColTwo[,1])
      DFinalII <- data.frame(matrix(DFII[,1], ncol = 4, byrow = TRUE))
      ColNames <- c ("SectionName", "CellSectionSA", "CellSectionVol", "SectionVerts")
      colnames(DFinalII) <- ColNames
    }
    
    # Merge cell distribution data frame with mitochondrial and contact distribution data by shared subcube vertices
    DFinal <- merge (DFinal, DFinalII [-1], by = "SectionVerts")
    
    # Compute total volume of mitochondria in cell
    DFinal$SectionVol <- as.numeric(DFinal$SectionVol)
    TotalVol <- sum(as.numeric(DFinal$SectionVol))
    TotalVol
    
    
    # Compute total volume of cell
    DFinal$CellSectionVol <- as.numeric(DFinal$CellSectionVol)
    CellTotalVol <- sum(as.numeric(DFinal$CellSectionVol))
    CellTotalVol
    
    # Initialize data frame to store metrics
    CubeNames <- data.frame ()
    PolarityFinal <- data.frame(matrix(ncol = 0, nrow = 2))
    AllVerts <-data.frame(matrix(ncol = 3, nrow = 0))
    
    n= DFinal$SectionVerts [1]
  
    # Iterate through each subcube section
    for (n in DFinal$SectionVerts){
      Counter = which(DFinal$SectionVerts == n)
      Counter
      # Extracting Mitochondria Volume in Section
      Vol <- as.numeric(DFinal$SectionVol [Counter])
      Vol
      
      # Compute volume percentage of mitochondria in subcube section
      PercentVol <- (Vol/TotalVol)*100
      PercentVol
      
      # Extracting Cell Volume in Section
      CellVol <- as.numeric(DFinal$CellSectionVol [Counter])
      CellVol
      
      # Compute volume percentage of cell in subcube section
      CellPercentVol <- (CellVol/CellTotalVol)*100
      CellPercentVol
      
      # Formatting vertices into numeric structure
      Verts <- DFinal$SectionVerts [Counter]
      Verts <- strsplit (Verts, ",")
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = " ", replacement = "", x = t))
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "\\[|\\]", replacement = "", x = t))
      Verts <- lapply(X = Verts, FUN = function(t) gsub(pattern = "[()]", replacement = "", x = t))
      Verts <- as.numeric(unlist(Verts))
      Verts <- sapply(Verts,round,2)
      VertsDF <- as.data.frame (Verts)
      VertsDF <-data.frame(matrix(VertsDF[,1], ncol = 3, byrow = TRUE))
      colnames(VertsDF) <- c("X", "Y", "Z")
      VertsDF$X <-paste0 (VertsDF$X, "X")
      VertsDF$Y <-paste0 (VertsDF$Y, "Y")
      VertsDF$Z <-paste0 (VertsDF$Z, "Z")
      VertsDF$n <- n
      
      
      # Extracting and formatting vertices of Cell bounding box
      BBVerts <- DFinal$`Subset$BBVerts.Cell` [Counter]
      BBVerts <- strsplit (BBVerts, ",")
      BBVerts <- lapply(X = BBVerts, FUN = function(t) gsub(pattern = " ", replacement = "", x = t))
      BBVerts <- lapply(X = BBVerts, FUN = function(t) gsub(pattern = "\\[|\\]", replacement = "", x = t))
      BBVerts <- lapply(X = BBVerts, FUN = function(t) gsub(pattern = "[()]", replacement = "", x = t))
      BBVerts <- as.numeric(unlist(BBVerts))
      BBVerts <- sapply(BBVerts,round,2)
      BBVertsDF <- as.data.frame (BBVerts)
      BBVertsDF <-data.frame(matrix(BBVertsDF[,1], ncol = 3, byrow = TRUE))
      colnames(BBVertsDF) <- c("X", "Y", "Z")
      BBVertsDF$X <-paste0 (BBVertsDF$X, "X")
      BBVertsDF$Y <-paste0 (BBVertsDF$Y, "Y")
      BBVertsDF$Z <-paste0 (BBVertsDF$Z, "Z")
      
      # Unlist vertices
      BBVerts<-unlist(as.list(BBVertsDF))
      Verts<-unlist(as.list(VertsDF [,-4]))
      
      # Create dataframe of all vertices
      AllVerts <- rbind (AllVerts, VertsDF)
      
      # If one of its vertex values is the same as a BB vertex value it is a corner (numbered by vertices) - 8
      if (nrow(semi_join(VertsDF,BBVertsDF)) > 0) {
        Designation <- c("Corner")
        Coordinates <- paste(unique(Verts[Verts %in% BBVerts])[1], unique(Verts[Verts %in% BBVerts])[2], unique(Verts[Verts %in% BBVerts])[3], sep = " ")
        Output <- c(Designation, Coordinates, n)
      }
      # If two of its vertex values lie along a BB edge (share a two coordinates (ex. x & y) that describe the BB box) it is an edge piece (numbered by edge) - 24
      if (length(unique(Verts[Verts %in% BBVerts])) == 2 ) {
        Designation <- c("Edge")
        Coordinates <- paste(unique(Verts[Verts %in% BBVerts])[1], unique(Verts[Verts %in% BBVerts])[2], sep = " ")
        Output <- c(Designation, Coordinates, n)
      }
      
      # If its set of vertex values share values with x, y, or z then it is a periphery piece - 24
      if (length (Verts[Verts %in% BBVerts])== 4) {
        Designation <- c("Face")
        Coordinates <- paste(unique(Verts[Verts %in% BBVerts])[1], sep = " ")
        Output <- c(Designation, Coordinates, n)
      }
      
      # If it shares no edges with BB it is a center (unnumbered) - 8
      if (length (Verts[Verts %in% BBVerts]) == 0) {
        Designation <- c("Center")
        Coordinates <- c("Center")
        Output <- c(Designation, Coordinates, n)
      }
      
      Output <- c(Output, PercentVol, CellPercentVol )
      CubeNames <- rbind (CubeNames, Output)
      colnames(CubeNames) <- c("Designation", "Coordinates", "SectionVerts", "PercentVol", "CellPercentVol")
      print (Counter)
    }
    
    # Merge All Details by full list of section verts
    DFinaler <- merge (DFinal, CubeNames, by= "SectionVerts") 
    
    
    # Now we can figure out polarity of the mitochondria within the cell using the CubeNames File
    FaceList <- DFinaler[DFinaler$Designation == "Face",] 
    FaceSum <- sum(as.numeric(FaceList$SectionVol)) 
    CellFaceSum <- sum(as.numeric(FaceList$CellSectionVol))
    NormFaceSum <- FaceSum/CellFaceSum
    MitoFaceSumSA <- sum(as.numeric(FaceList$SectionMitoSA))
    
    
    EdgeList <- DFinaler[DFinaler$Designation == "Edge",]
    EdgeSum <- sum(as.numeric(EdgeList$SectionVol))
    CellEdgeSum <- sum(as.numeric(EdgeList$CellSectionVol))
    NormEdgeSum <- EdgeSum/CellEdgeSum
    MitoEdgeSumSA <- sum(as.numeric(EdgeList$SectionMitoSA))
    
    CornerList <- DFinaler[DFinaler$Designation == "Corner",]
    CornerSum <- sum(as.numeric(CornerList$SectionVol))
    CellCornerSum <- sum(as.numeric(CornerList$CellSectionVol))
    NormCornerSum <- CornerSum/CellCornerSum
    MitoCornerSumSA <- sum(as.numeric(CornerList$SectionMitoSA))
    
    CenterList <- DFinaler[DFinaler$Designation == "Center",]
    CenterSum <- sum(as.numeric(CenterList$SectionVol))
    CellCenterSum <- sum(as.numeric(CenterList$CellSectionVol))
    NormCenterSum <- CenterSum/CellCenterSum
    MitoCenterSumSA <- sum(as.numeric(CenterList$SectionMitoSA))
    
    
    Metrics <- cbind (FaceSum, EdgeSum, CornerSum, CenterSum, CellFaceSum, CellEdgeSum, CellCornerSum, CellCenterSum, NormFaceSum, NormEdgeSum, NormCornerSum, NormCenterSum, MitoFaceSumSA, MitoEdgeSumSA, MitoCornerSumSA, MitoCenterSumSA, File)
    AllMetrics <- rbind (AllMetrics, Metrics)
  }
  
  
  
  MitoCubes <- DFinaler [as.numeric(DFinaler$SectionVol) >0,]
  
  
  FileName<-gsub("Distribution.csv", "", i)
  
  OverlapFile <- paste0(FileName, "Overlap.csv") 
 
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
    
    str <- lapply(X =paste0 (FileName, "Overlap.csv"), FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    str <- stri_split_fixed(str,"-",n = 3)
    str <- data.frame (str)
    File <- as.character (str [3,1])
    Condition <- gsub("-.*","",File)
    Condition <- gsub("^\\d+|\\d+$", "", Condition)
    SubStr <- paste(Condition, "-", sep="")
    CellID <- gsub(SubStr,"", File)
    OverlapFileFinal <- cbind (File, OverlapFileFinal)
    
    OverlapDeetsDF <-data.frame(matrix(ncol = 3, nrow = 0))
    Metrics <-data.frame(matrix(ncol = 6, nrow = 0))
    MasterMito <- data.frame(matrix(ncol = 16, nrow = 0))
    
    for (n in MitoCubes$SectionVerts){
      print (n)
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
        if (BBVolume>0) {
          if (FractionBBOverlap > 0) {
            print (FractionBBOverlap)
            OverlapDeet<- as.data.frame (t(c(FractionBBOverlap, i, n)))
            colnames(OverlapDeet) <- c("FractionBBOverlap", "BBVerts", "SectionVerts")
            OverlapDeetsDF <-rbind(OverlapDeetsDF, OverlapDeet)
          }
        }
      }
      colnames(OverlapDeetsDF) <- c("FractionBBOverlap", "BBVerts", "SectionVerts")
      MitoCubesWithContact <- merge (MitoCubes, OverlapDeetsDF, by= "SectionVerts")
      MitoCubesWithContact <- merge (MitoCubesWithContact,OverlapFileFinal [-1], by = "BBVerts")
      ##Here is where to put filters for Overlap data
    }
    
    MitoCubesWithContact$TrueSA <-MitoCubesWithContact$SurfaceArea * as.numeric(MitoCubesWithContact$FractionBBOverlap)
    MasterMito <- rbind (MasterMito, MitoCubesWithContact)
    ContactPeriphSum <-sum (MitoCubesWithContact[MitoCubesWithContact$Designation %in% c("Face", "Corner", "Edge"), "TrueSA"])
    ContactCenterSum <-sum (MitoCubesWithContact[MitoCubesWithContact$Designation %in% c("Center"), "TrueSA"])
    TotalContactSum <- sum (MitoCubesWithContact$TrueSA)
    ContactPeriphPercent <- ContactPeriphSum/TotalContactSum*100
    ContactCenterPercent <- ContactCenterSum/TotalContactSum*100
    MitoPeriphPercentSA <-sum (DFinaler[DFinaler$Designation %in% c("Face", "Corner", "Edge"), "PercentSectionMitoSA"])
    MitoCenterPercentSA <-sum (DFinaler[DFinaler$Designation %in% c("Center"), "PercentSectionMitoSA"])
    Metric <- as.data.frame (t(c(File, ContactPeriphSum, ContactCenterSum, TotalContactSum, ContactPeriphPercent, ContactCenterPercent)))
    colnames(Metric) <-c("File", "ContactPeriphSum", "ContactCenterSum", "TotalContactSum", "ContactPeriphPercent", "ContactCenterPercent")
    Metric <- merge (AllMetrics, Metric, by= "File")
    MasterMetrics <- rbind (MasterMetrics, Metric)
  }
  if (OverlapFile %in% EmptyOverlapFiles == "TRUE") {
   MasterMetrics <- rbind (MasterMetrics, AllMetrics)
  }
  
}



# Adding distribution metrics into CellStats dataframe
CellStats <- merge (CellStats, MasterMetrics, by= "File")
CellStats <- unique (CellStats)

# Computing Periphery Sum, and Normalized Periphery Sum
CellStats$PeripherySum <- (as.numeric(CellStats$FaceSum) + as.numeric(CellStats$EdgeSum) + as.numeric(CellStats$CornerSum))
CellStats$NormPeripherySum <- (as.numeric(CellStats$NormFaceSum) + as.numeric(CellStats$NormEdgeSum) + as.numeric(CellStats$NormCornerSum))

# Get Overlap (Contact Sites) Files and collate into dataframe
OverlapStatsFiles<-grep("Overlap.csv",names(.GlobalEnv),value=TRUE)
OverlapStats <- data.frame ()

CutoffOverlapStats = data.frame ("File", "TotalContacts", "TotalSurfaceArea", "TotalVolume", "FirstCutOffContacts", "FirstCutOffSurfaceArea", "FirstCutOffVolume","SecondCutOffContacts", "SecondCutOffSurfaceArea", "SecondCutOffVolume","ThirdCutOffContacts", "ThirdCutOffSurfaceArea", "ThirdCutOffVolume")
colnames(CutoffOverlapStats) <- c("File", "TotalContacts", "TotalSurfaceArea", "TotalVolume", "FirstCutOffContacts", "FirstCutOffSurfaceArea", "FirstCutOffVolume","SecondCutOffContacts", "SecondCutOffSurfaceArea", "SecondCutOffVolume","ThirdCutOffContacts", "ThirdCutOffSurfaceArea", "ThirdCutOffVolume")
AllContacts = data.frame ("Condition", "ContactSA", "ContactVol")
colnames(AllContacts) <- c("Condition", "ContactSA", "ContactVol")
CutOffContacts = data.frame ("Condition", "ContactSA", "ContactVol")
colnames(CutOffContacts) <- c("Condition", "ContactSA", "ContactVol")

# Iterate through each Overlap (Contact Site) File
for(i in OverlapStatsFiles){
  if (i %in% EmptyOverlapFiles == "FALSE"){
    
    # Read in Overlap (Contact Site) File
    # And Format Strings
    DF <- get(i)
    RowOne <- colnames(DF)
    ColTwo <- rbind (RowOne,DF[1])
    DF <- as.data.frame (ColTwo[,1])
    DFinal <- data.frame(matrix(DF[,1], ncol = 3, byrow = TRUE))
    
    ColNames <- c ("SurfaceArea", "Volume", "BBVerts")
    colnames(DFinal) <- ColNames
    
    str <- DFinal [1]
    str <- lapply(X = str, FUN = function(t) gsub(pattern = ".*:", replacement = "", x = t))
    str <- lapply(X = str, FUN = function(t) gsub(pattern = "^\\D+", replacement = "\\1", x = t))
    str <- lapply(X =str, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    str <-lapply(X =str, FUN = function(t) gsub(pattern = "e.", replacement = "e-", x = t))
    DFinal [1] <-as.numeric (unlist(str))
    
    str <- lapply(X =i, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
    str <- stri_split_fixed(str,"-",n = 3)
    str <- data.frame (str)
    File <- as.character (str [3,1])
    Condition <- gsub("-.*","",File)
    Condition <- gsub("^\\d+|\\d+$", "", Condition)
    SubStr <- paste(Condition, "-", sep="")
    CellID <- gsub(SubStr,"", File)
    DFinal <- cbind (File, DFinal)
    
    # Initialize Variables
    TotalContacts = 0
    FirstCutOffContacts = 0
    SecondCutOffContacts = 0
    ThirdCutOffContacts = 0
    TotalContactSurfaceArea = 0
    TotalContactVolume = 0
    OverlapSurfaceArea1 = 0
    OverlapVolume1 = 0
    OverlapSurfaceArea2 = 0
    OverlapVolume2 = 0
    OverlapSurfaceArea3 = 0
    OverlapVolume3 = 0
    
    # Iterate through each row of Overlap (Contact Site) File
    for (i in DFinal$SurfaceArea ) {
      # Computing Stats
      TotalContacts = TotalContacts + 1
      TotalContactSurfaceArea = TotalContactSurfaceArea + as.numeric (i)
      TotalContactVolume = TotalContactVolume + as.numeric (DFinal$Volume[DFinal$SurfaceArea == i])
      AllContactsCount <- c(Condition, i, DFinal$Volume[DFinal$SurfaceArea == i])
      AllContacts <- rbind (AllContacts, as.numeric (AllContactsCount))
      
      # Applying Cutoffs
      if (i >= FirstCutOff){
        FirstCutOffContacts = as.numeric (FirstCutOffContacts + 1)
        OverlapSurfaceArea1 = as.numeric (OverlapSurfaceArea1 + as.numeric (i))
        OverlapVolume1 = as.numeric (OverlapVolume1 + as.numeric (DFinal$Volume[DFinal$SurfaceArea == i]))
        CutOffContactsCount <- c(Condition, i, DFinal$Volume[DFinal$SurfaceArea == i])
        CutOffContacts <- rbind (CutOffContacts, as.numeric (CutOffContactsCount))
      }
      if (i >= SecondCutOff){
        SecondCutOffContacts = as.numeric (SecondCutOffContacts + 1)
        OverlapSurfaceArea2 = as.numeric (OverlapSurfaceArea2 + as.numeric (i))
        OverlapVolume2 = as.numeric (OverlapVolume2 + as.numeric (DFinal$Volume[DFinal$SurfaceArea == i]))
      }
      if (i >= ThirdCutOff){
        ThirdCutOffContacts = as.numeric (ThirdCutOffContacts + 1)
        OverlapSurfaceArea3 = as.numeric (OverlapSurfaceArea3 + as.numeric (i))
        OverlapVolume3 = as.numeric (OverlapVolume3 + as.numeric (DFinal$Volume[DFinal$SurfaceArea == i]))
      }
    }
    
    # Collate Stats
    Stats <- c(File, TotalContacts, TotalContactSurfaceArea, TotalContactVolume, FirstCutOffContacts, OverlapSurfaceArea1, OverlapVolume1, SecondCutOffContacts, OverlapSurfaceArea2, OverlapVolume2, ThirdCutOffContacts,OverlapSurfaceArea3,OverlapVolume3)
    CutoffOverlapStats = rbind (CutoffOverlapStats,Stats)
  }
}

# Extract Mitochondria Files
MitoStatsFiles<-grep("MitoComponents.csv",names(.GlobalEnv),value=TRUE)
FullMitoStats <- data.frame ("File", "TotalMitoComponents","TotalMitoVolume", "MitoCutOffComponents", "MitoCutOffVolume")
colnames(FullMitoStats) <- c("File", "TotalMitoComponents","TotalMitoVolume", "MitoCutOffComponents", "MitoCutOffVolume")


i = MitoStatsFiles [1]
# Iterate through each Mitochondria File
for(i in MitoStatsFiles){
  # Get file and format strings
  #Getting mitochondrial data from the file#
  DF <- get(i)
  DF <- DF [1]
  RowOne <- colnames(DF [1])
  RowOne <- gsub("..", ",", RowOne, fixed = TRUE)
  RowOne <- gsub("e.", "e-", RowOne, fixed = TRUE)
  RowOne <- strsplit(RowOne, ",")
  RowOne <- lapply(X =RowOne, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
  RowOne <- lapply(X = RowOne, FUN = function(t) gsub(pattern = "^\\D+", replacement = "\\1", x = t))
  RowOne <- as.numeric(unlist(RowOne))
  MitoStats <- rbind (DF, RowOne)
  colnames(MitoStats) <- c("Volume")
  MitoStats$Volume <- as.numeric (MitoStats$Volume)
  
  #Getting file name for mitochondrial data#
  str <- lapply(X =i, FUN = function(t) gsub(pattern = "(\\d)[^0-9]+$", replacement = "\\1", x = t))
  str <- stri_split_fixed(str,"-",n = 3)
  str <- data.frame (str)
  File <- as.character (str [3,1])
  MitoCutOff= 1e-04
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
}

FullMitoStats

# Merging all stats into one dataframe
SummaryFile <- merge(CellStats,FullMitoStats[-1,],by="File")
SummaryFile <- unique (SummaryFile)

if (nrow(CutoffOverlapStats)> 1){
  SummaryFile <- merge(SummaryFile,CutoffOverlapStats[-1,],by="File")
}

row.names (which(SummaryFile == "NaN", arr.ind = TRUE))
NANValues <- unique (as.numeric (row.names (which(SummaryFile == "NaN", arr.ind = TRUE))))
NAValues <- unique (as.numeric (row.names (which(SummaryFile == "NA", arr.ind = TRUE))))
NAValues <- c(NAValues,NANValues)

if (length (NAValues)>0){
  SummaryFile[-c(NAValues), ]
  SimpleSummary <- SummaryFile[-c(NAValues), ]
}
if (length (NAValues)==0){
  SimpleSummary <- SummaryFile
}


# Changing everything that's number to be numeric
SimpleSummary[,!colnames(SimpleSummary) %in% c("File", "CellID", "Condition", "CenterOfGravity.Mitochondria", "BBVerts.Mitochondria","CenterOfGravity.SubcellularStructure", "BBVerts.SubcellularStructure", "CenterOfGravity.MitoNodes", "BBVerts.MitoNodes", "CenterOfGravity.ER", "BBVerts.ER", "CenterOfGravity.ERNodes", "BBVerts.ERNodes", "CenterOfGravity.Cell", "BBVerts.Cell")] <- mutate_all(SummaryFile[,!colnames(SummaryFile) %in% c("File", "CellID", "Condition", "CenterOfGravity.Mitochondria", "BBVerts.Mitochondria", "CenterOfGravity.SubcellularStructure", "BBVerts.SubcellularStructure", "CenterOfGravity.MitoNodes", "BBVerts.MitoNodes", "CenterOfGravity.ER", "BBVerts.ER", "CenterOfGravity.ERNodes", "BBVerts.ERNodes", "CenterOfGravity.Cell", "BBVerts.Cell")], function(x) as.numeric(as.character(x)))


if (nrow(CutoffOverlapStats)> 1){
  # Dividing surface area by two to prevent double counting of Mito-ER contact area
  SimpleSummary$HalfOverlapSA <- SimpleSummary$TotalSurfaceArea/2
  
  # Percent of mitochondria surface area that is in contact with ER
  SimpleSummary$MitoOverlap <- (SimpleSummary$HalfOverlapSA/SimpleSummary$SurfaceArea.Mitochondria)*100 
  
  # Percent of ER surface area that is in contact with Mitochondria
  SimpleSummary$EROverlap <- (SimpleSummary$HalfOverlapSA/SimpleSummary$SurfaceArea.ER)*100 
  
  # ER volume normalized by cell volume
  SimpleSummary$NormalizedERVol <-(SimpleSummary$Volume.ER/SimpleSummary$Volume.Cell)*100
  
  #Peripheral contact surface area normalized to peripheral mitochondrial surface area
  SimpleSummary$NormPeriphDistContactSA <- (SimpleSummary$ContactPeriphSum/(SimpleSummary$MitoFaceSumSA+ SimpleSummary$MitoEdgeSumSA + SimpleSummary$MitoCornerSumSA))
  
  SimpleSummary$NormCenterDistContactSA <- (SimpleSummary$ContactCenterSum/SimpleSummary$MitoCenterSumSA)
  
  
  SimpleSummary$ContactsSANormToMitoSA<- (SimpleSummary$HalfOverlapSA/ SimpleSummary$SurfaceArea.Mitochondria)
  
}


# Mitochondrial volume normalized by cell volume
SimpleSummary$NormalizedMitoVol <-(SimpleSummary$Volume.Mitochondria/SimpleSummary$Volume.Cell)*100
SimpleSummary$NormalizedMitoVol <-(SimpleSummary$Volume.SubcellularStructure/SimpleSummary$Volume.Cell)*100

# Ratio of Peripheral mitochondrial volume % over peripheral cell volume %
SimpleSummary$NormPeriphDistMito <- SimpleSummary$PeripherySum/ (SimpleSummary$CellFaceSum + SimpleSummary$CellFaceSum + SimpleSummary$CellEdgeSum)

# Ratio of Center mitochondrial volume % over Center cell volume %
SimpleSummary$NormCenterDistMito <- SimpleSummary$CenterSum/SimpleSummary$CellCenterSum


SimpleSummary$MeanCellRadius <-((SimpleSummary$BBHeight.Cell+SimpleSummary$BBLength.Cell+ SimpleSummary$BBWidth.Cell)/3)/2


SimpleSummary$AsymmetryMetric <- (SimpleSummary$COGDifference.Mitochondria/ SimpleSummary$MeanCellRadius)


SimpleSummary$AsymmetryMetric <- (SimpleSummary$COGDifference.SubcellularStructure/ SimpleSummary$MeanCellRadius)


SimpleSummary$SurfaceAreaToVolume <- SimpleSummary$SurfaceArea.SubcellularStructure/ SimpleSummary$Volume.SubcellularStructure


SimpleSummary$ContactsNormToMitoSA <- (SimpleSummary$TotalContacts/ SimpleSummary$SurfaceArea.Mitochondria)






# SplitDataByStrains
##Get strains/condition from file name##
SimpleSummary$Condition <- SimpleSummary$File
SimpleSummary$Condition[grepl("NK7", SimpleSummary$Condition)] <- "mgm1"
SimpleSummary$Condition[grepl("WT", SimpleSummary$Condition)] <- "WT"
SimpleSummary$Condition[grepl("NK15", SimpleSummary$Condition)] <- "mdm36"
SimpleSummary$Condition[grepl("NK2", SimpleSummary$Condition)] <- "fis1"
SimpleSummary$Condition[grepl("NK13", SimpleSummary$Condition)] <- "mdm34"
SimpleSummary$Condition[grepl("NK4", SimpleSummary$Condition)] <- "mdm10"
SimpleSummary$Condition[grepl("NK16", SimpleSummary$Condition)] <- "mdm37"
SimpleSummary$Condition[grepl("NK18", SimpleSummary$Condition)] <- "mdm39"
SimpleSummary$Condition[grepl("NK21", SimpleSummary$Condition)] <- "num1"


SimpleSummary$Condition <- SimpleSummary$File
SimpleSummary$Condition[grepl("GEtoGE", SimpleSummary$Condition)] <- "Respiration"
SimpleSummary$Condition[grepl("GlutoGE", SimpleSummary$Condition)] <- "Transition"
SimpleSummary$Condition[grepl("GlutoGlu", SimpleSummary$Condition)] <- "Fermentation"


Strains <- split(SimpleSummary, SimpleSummary$Condition)
Strains
Counter<-(length(Strains))
list2env(Strains, envir = .GlobalEnv)

# Determining outliers based on mitochondrial volume as a function of cell volume
i <- 2
OutlierFree <- data.frame()
length ((colMeans(SimpleSummary[sapply(SimpleSummary, is.numeric)])))


RowNumber<- length ((colMeans(SimpleSummary[sapply(SimpleSummary, is.numeric)])))
Means <- data.frame(matrix(0, ncol = 0, nrow = RowNumber))

# Iterate over strains
while (i <= Counter) {
  # ~Formatting~
  DF <- as.list (Strains [i])
  DF
  names_of_dataframes <- ls.str(Strains, mode = "list")
  names_of_dataframes[i]
  DF <- as.data.frame(Strains [i])
  Names <- colnames(DF)
  ColumnNames <- gsub("^.*?\\.","", Names)
  colnames(DF) <- c(ColumnNames)
  drops <- c("File", "CellID", "Condition", "CenterOfGravity.Mitochondria", "BBVerts.Mitochondria", "CenterOfGravity.MitoNodes", "BBVerts.MitoNodes", "CenterOfGravity.ER", "BBVerts.ER", "CenterOfGravity.ERNodes", "BBVerts.ERNodes", "CenterOfGravity.Cell", "BBVerts.Cell","CenterOfGravity.SubcellularStructure", "BBVerts.SubcellularStructure" )
  Values <- DF[ , !(names(DF) %in% drops)]
  for (n in colnames(Values)){
    print (n)
    DF [n] <- sapply(DF[n],as.numeric)
  }
  
  boxplot(DF$NormalizedMitoVol)
  
  boxplot.stats(DF$NormalizedMitoVol)$out
  out <- boxplot.stats(DF$NormalizedMitoVol)$out
  out_ind <- which(DF$NormalizedMitoVol %in% c(out))
  
  # ~Outlier Removal~
  K <- length (out_ind)
  #if (K > 0) {
   # test <- rosnerTest(DF$NormalizedMitoVol,
                       #k = length (out_ind))
    #DF2 <- test$all.stats
    #Values2 <- DF2$Value [DF2$Outlier == TRUE]
    #Removed <- DF
    #Removed <- DF[!DF$NormalizedMitoVol%in%Values2,]
    #Mean <- data.frame(colMeans(Removed[sapply(Removed, is.numeric)]))
    #colnames(Mean)<- names_of_dataframes[i]
    #OutlierFree <- rbind(OutlierFree, Removed)
   # Means <- cbind(Means, Mean)
    #i = i+1
  #}
  if (K >= 0) {
    Removed <- DF
    Mean <- data.frame(colMeans(Removed[sapply(Removed, is.numeric)]))
    colnames(Mean)<- names_of_dataframes[i]
    OutlierFree <- rbind(OutlierFree, Removed)
    Means <- cbind(Means, Mean)
    i = i+1
  }
  
}


# Output CSV Summary File
write.csv(SimpleSummary,paste0(getwd(),"CellStatsSummary.csv"), row.names = FALSE)


# Average all measurements and drop NA values
Means <- Means [,colSums(is.na(Means))<nrow(Means)]

Means
MeansT <- data.frame(t(Means))
Names <- rownames(MeansT)
MeansT <- cbind (Names, MeansT)
colnames(MeansT)[colnames(MeansT) == "Names"] <- "Condition"

# Fin #



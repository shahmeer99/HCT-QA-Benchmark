
# CLEAN UP DATA REMOVING FILES (Tables, Qand A which do not come with all versions zzz_HCT, zzz_DB, zzz_QandA)




################################################################################
################################################################################
################################################################################
################################################################################
rm(list = ls())
set.seed(0) #set.seed(0) 

library(jsonlite)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
library(magick)  # pdf to png
library(readr)

source("config.R")
source("toolboxTABLESgenerator.R")

################ I/O #####################

PARAMETERSfolder = PARAMETERS_FOLDER
semTABLESfolder = SEMANTIC_TABLES_FOLDER
semQandAfolder = SEMANTIC_QANDA_FOLDER
nonsemTABLESfolder = NON_SEMANTIC_TABLES_FOLDER
nonsemQandAfolder = NON_SEMANTIC_QANDA_FOLDER

output_folder = BENCHMARK_FOLDER

# create the output folder if it does not exist
ifelse(!dir.exists(file.path(output_folder)),
       dir.create(file.path(output_folder)),
       "")


### get all root filenames
suffixStr = "_QandA.json" 
Lfiles = list.files(semQandAfolder)
Lfilenames = Lfiles[grepl(suffixStr,Lfiles,fixed=TRUE)]

# process tables/Q/A of each root filename
nFilenames = length(Lfilenames)
ll = 0
LfilenamesToKeep = NULL
for (kk in 1:nFilenames){
  
  print(paste0("EXISTS? ", kk,"/",nFilenames," -- ", 100*kk/nFilenames,"%"))
  # read semantic names from a series
  
  fileNameRoot = unlist(strsplit(Lfilenames[kk],suffixStr))
 
  # check all versions exist
  if (file.exists(paste0(nonsemQandAfolder,fileNameRoot,"_QandA_NONSEM.json")) &&
      file.exists(paste0(semTABLESfolder,fileNameRoot,"_HCT.html")) &&
      file.exists(paste0(semTABLESfolder,fileNameRoot,"_HCT.csv")) &&
      file.exists(paste0(semTABLESfolder,fileNameRoot,"_DB.html")) &&
      file.exists(paste0(semTABLESfolder,fileNameRoot,"_DB.csv")) &&
      file.exists(paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.html")) &&
      file.exists(paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.csv")) &&
      file.exists(paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.html")) &&
      file.exists(paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.csv")) ){
    ll = ll+1
    LfilenamesToKeep[ll] = fileNameRoot
  }
  
}

nFilenamesToKeep = length(LfilenamesToKeep)
for (kk in 1:nFilenamesToKeep){
  
  print(paste0("COPY ",kk,"/",nFilenamesToKeep," -- ", 100*kk/nFilenamesToKeep,"%"))
  # read semantic names from a series
  
  fileNameRoot = LfilenamesToKeep[kk]
  
  # copy QandA in Benchmark folder
  file.copy(paste0(semQandAfolder,fileNameRoot,"_QandA.json"), output_folder)
  file.copy(paste0(nonsemQandAfolder,fileNameRoot,"_QandA_NONSEM.json"), output_folder)
  
  # copy table in Benchmark folder
  file.copy(paste0(semTABLESfolder,fileNameRoot,"_HCT.html"), output_folder)
  file.copy(paste0(semTABLESfolder,fileNameRoot,"_HCT.csv"), output_folder)
  file.copy(paste0(semTABLESfolder,fileNameRoot,"_DB.html"), output_folder)
  file.copy(paste0(semTABLESfolder,fileNameRoot,"_DB.csv"), output_folder)
  file.copy(paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.html"), output_folder)
  file.copy(paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.csv"), output_folder)
  file.copy(paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.html"), output_folder)
  file.copy(paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.csv"), output_folder)
  
  
}
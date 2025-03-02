################################################################################
### 4 - GENERATE NON-SEMANTIC VERSIONS OF ALL TABLES AND QUESTIONS (ANSWERS ARE KEPT IDENTICAL)

### for each root name xx_set_n
### Read DB table xx_set_n_DB.csv as it contains both SQL and HCT names
### Extract semantic names, generate a non semantic name, create a dictionary.
### substitutes semantic names with random consonent-only names in all files:
# xx_set_n_DB.html
# xx_set_n_DB.csv
# xx_set_n_HCT.html
# xx_set_n_HCT.json
# xx_set_n_HCT.csv
# xx_set_n_QandA.json

### NOTE: non-semantic code names are shared only within the series with the same root name xx_set_n

################################################################################

rm(list = ls())
set.seed(0)  

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
nonsemQandAfolder = NON_SEMANTIC_QANDA_FOLDER  ## OUTPUT
nonsemTABLESfolder = NON_SEMANTIC_TABLES_FOLDER  ## OUTPUT

# create the output folder if it does not exist
ifelse(!dir.exists(file.path(nonsemQandAfolder)),
       dir.create(file.path(nonsemQandAfolder)),
       "")

# create the output folder if it does not exist
ifelse(!dir.exists(file.path(nonsemTABLESfolder)),
       dir.create(file.path(nonsemTABLESfolder)),
       "")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
replaceSemantics <- function(strIn,dictCodeNames){
  
  strOut = strIn
  for (i in 1:nrow(dictCodeNames)){
    key = dictCodeNames$sem[i]
    val = dictCodeNames$nonsem[i]
    strOut <- gsub(paste0("\\b",key,"\\b"),val,strOut)
  }
  strOut <- gsub("\r","",strOut) # remove unnecessary carriage return \r\n -> \n
  
  return(strOut) 
}

################################################################################
geneNonsemanticCodes <- function(numCodes){
  
  # generate non semantic codes
  consonants <- c("b","c","d","f","g","h","j","k","l","m","n","p","q",
                  "r","s","t","v","w","x","y","z")
  allNonSemanticCodes = t(combn(consonants,2))
  k = 2
  for (tt  in 1:10){
    if (nrow(allNonSemanticCodes) < numCodes) {
      k = k + 1
      allNonSemanticCodes = t(combn(consonants,k))
    }
    
  }
  nbCodes = nrow(allNonSemanticCodes)
  nonSemanticCodes = rep("a",nbCodes)
  for (icode in 1:nrow(allNonSemanticCodes)){
    nonSemanticCodes[icode] = paste0(allNonSemanticCodes[icode,],collapse = "")
  }
  
  # random permutation of the non semantic codes
  randNonSemanticCodes = sample(nonSemanticCodes) 
  
  codeNames = randNonSemanticCodes[1:nbCodes]
  return(codeNames)
}

################################################################################
getCodeNamesDictFromDBtable <- function(DBtable){
  
  # assume format:
  # feat1 feat2 ... featN   Value
  # DBft1 DBft2 ... DBfeatN Value  --> names used for SQL query
  # v1ft1 v1ft2 ... v1ftN    123
  # ...
  # vMft1 vMft2 ... vMftN    456
  
  # extract all semantic except Value column
  DBtableNoVal = DBtable[,1:(ncol(DBtable)-1)]
  
  numCodes = (nrow(DBtableNoVal)+1)*ncol(DBtableNoVal)
  nonsemCodes = geneNonsemanticCodes(numCodes)[1:numCodes] # generate enough codes
  nonsemCodeMatrix = matrix(nonsemCodes, nrow(DBtableNoVal)+1,ncol(DBtableNoVal)) 
                        
  nonsemFEATnames = c("AA","BB","CC","DD","EE","FF","GG","HH","JJ","KK","LL","MM")
  # for each column
  semNames = NULL
  nonsemNames = NULL
  for (i in 1:ncol(DBtableNoVal)){
    DBcol = DBtableNoVal[,i]
    FEATnameHCT = gsub("_"," ",DBcol[1])
    FEATnameDB = DBtableNoVal[1,i]
    
    semanticValsCol = unique(DBcol[2:length(DBcol)])
    
    nonsemCodesCol = paste0(nonsemFEATnames[i],nonsemCodeMatrix[1:length(semanticValsCol),i])
    
    semNames = c(semNames, c(FEATnameHCT,FEATnameDB,semanticValsCol))
    nonsemNames = c(nonsemNames, c(nonsemFEATnames[i],nonsemFEATnames[i],nonsemCodesCol))
  }
  
  
  
  dictCodeNames = data.frame(sem = semNames, nonsem = nonsemNames, stringsAsFactors = FALSE)
  
   
  return(dictCodeNames)
}

################################################################################
################################################################################
################################################################################
################################################################################

### get all root filenames
suffixStr = "_QandA.json" 
Lfiles = list.files(semQandAfolder)
Lfilenames = Lfiles[grepl(suffixStr,Lfiles,fixed=TRUE)]

# process tables/Q/A of each root filename
nFilenames = length(Lfilenames)
for (kk in 1:nFilenames){
  
  print(paste0(kk,"/",nFilenames," -- ", 100*kk/nFilenames,"%"))
  # read semantic names from a series
  
  fileNameRoot = unlist(strsplit(Lfilenames[kk],suffixStr))
  
  ######### READ SEMANTIC DATA
  
  # read DB table csv file
  DBtable = read.csv(paste0(semTABLESfolder,fileNameRoot,"_DB.csv"),stringsAsFactors = FALSE)
  
  # "Hotel" "dgs"
  dictCodeNames = getCodeNamesDictFromDBtable(DBtable)
  
  #####################
  # xx_set_n_DB.html
  # xx_set_n_DB.csv
  # xx_set_n_HCT.html
  # xx_set_n_HCT.csv
  # xx_set_n_QandA.json
  
  ##################### 
  ##################### xx_set_n_QandA.json
  ##################### 
  fileIN = paste0(semQandAfolder,fileNameRoot,"_QandA.json")
  fileOUT = paste0(nonsemQandAfolder,fileNameRoot,"_QandA_NONSEM.json")
  
  strIn = read_file(fileIN)
  strOut = replaceSemantics(strIn,dictCodeNames)
  write.table(strOut, file = fileOUT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
  
  ##################### 
  ##################### xx_set_n_DB.html
  ##################### 
  fileIN = paste0(semTABLESfolder,fileNameRoot,"_DB.html")
  fileOUT = paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.html")
  
  strIn = read_file(fileIN)
  strOut = replaceSemantics(strIn,dictCodeNames)
  write.table(strOut, file = fileOUT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
  
  ##################### 
  ##################### xx_set_n_DB.csv
  ##################### 
  fileIN = paste0(semTABLESfolder,fileNameRoot,"_DB.csv")
  fileOUT = paste0(nonsemTABLESfolder,fileNameRoot,"_DB_NONSEM.csv")
  
  strIn = read_file(fileIN)
  strOut = replaceSemantics(strIn,dictCodeNames)
  write.table(strOut, file = fileOUT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
  
  ##################### 
  ##################### xx_set_n_HCT.csv
  ##################### 
  fileIN = paste0(semTABLESfolder,fileNameRoot,"_HCT.csv")
  fileOUT = paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.csv")
  
  strIn = read_file(fileIN)
  strOut = replaceSemantics(strIn,dictCodeNames)
  write.table(strOut, file = fileOUT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
  
  ##################### 
  ##################### xx_set_n_HCT.html
  ##################### 
  fileIN = paste0(semTABLESfolder,fileNameRoot,"_HCT.html")
  fileOUT = paste0(nonsemTABLESfolder,fileNameRoot,"_HCT_NONSEM.html")
  
  strIn = read_file(fileIN)
  strOut = replaceSemantics(strIn,dictCodeNames)
  write.table(strOut, file = fileOUT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
  
}

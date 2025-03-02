################################################################################
### 2 - GENERATE TABLE INSTANCES FROM TABLE TEMPLATES


rm(list = ls())
set.seed(0) #set.seed(0) 
library(jsonlite)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
library(magick)  # pdf to png

source("config.R")
source("toolboxTABLESgenerator.R")

################ I/O #####################

inputFolder  = PARAMETERS_FOLDER
outputFolder = SEMANTIC_TABLES_FOLDER
semanticsJSONfile = PARAM_SEMANTICS_JSON # file containing all semantic data
tablesJSONfile = PARAM_TABLE_TO_GEN_JSON # file containing all table patterns to generate


# create the output folder if it does not exist
ifelse(!dir.exists(file.path(outputFolder)),
       dir.create(file.path(outputFolder)),
       "")


DO_NOT_SAVE = FALSE # set to TRUE for debug, Automatically set to TRUE if warning message
DO_NOT_SAVE_DB_HTML = FALSE # set to TRUE for debug, Automatically set to TRUE if warning message
DO_NOT_GENERATE_PIVOT = FALSE # set to TRUE for debug


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


SIGNATURES <- NULL

# read semantic data
allSemanticAttributes <- read_json(paste0(inputFolder,semanticsJSONfile))

# read table instructions data
allTablesJSON <- read_json(paste0(inputFolder,tablesJSONfile)) 

totalNumTableGene = 0
allPivot <- NULL
allPivot$df <- list()
allPivot$signature <- list()

tableJSONname = NULL
for (itab in 1:length(allTablesJSON)){
  tableJSONname[itab] = allTablesJSON[[itab]]$name
}

# generate each table
numTables = length(allTablesJSON)


for (itab in 1:numTables){

  tableJSON = allTablesJSON[[itab]]
    
  outputNamePrefix = tableJSON$name
  
  # read the number of replica to generate
  numReplica = tableJSON$replica
  agg_fun = tableJSON$agg_fun1
  agg_name = tableJSON$agg_name1
  tableTitle = tableJSON$valueName # title of the table
  
  curReplica = 0
  
  trialCnt = 0
  trialCntMAX = 10
  geneNextReplica = TRUE
  
  
  #for (curReplica in 1:numReplica){
  while (geneNextReplica){
    
    
    curReplica = curReplica + 1
    
    # set seed to allow debugging if some replica are wrong
    seedValue = (itab-1)*numReplica + curReplica
    set.seed(seedValue) 
    
    print(paste0("NAME: ", outputNamePrefix,"_",curReplica," Generate DB table: ",itab,"/",numTables," -- Replica: ",curReplica,"/",numReplica))
    
    if (curReplica == numReplica) geneNextReplica = FALSE
    
    
    # get list of attributes for the DB table
    
    # read all column attributes (assume a single group of attributes)
    groupInd = 1
    
    L <- NULL
    DO_AGG <- NULL
    colCodes <- NULL
    colCodeNames <- NULL
    rowCodes <- NULL
    rowCodeNames <- NULL
    
    
    rowTableFormat = tableJSON$row_format
    colAttr = tableJSON$columns$groups[[groupInd]]$attributes
    rowAttr = tableJSON$rows$groups[[groupInd]]$attributes
    
    for (j in 1:length(colAttr)){
      colCode = unlist(colAttr[[j]]$code)
      valKeep = unlist(colAttr[[j]]$keep)
      valRemove = unlist(colAttr[[j]]$remove)
      valSample = unlist(colAttr[[j]]$sample)
      isSymmetric = unlist(colAttr[[j]]$symmetric)
      if (length(isSymmetric)==0) isSymmetric="true"
      
      namesAndValues = getNamesValues(allSemanticAttributes$data,colCode)
      
      
      depthAttr = length(namesAndValues$names)
      if (depthAttr>1){
        # multilevel attribute 
        # asymmetric/symmetric options is ignored
        # it is necessarily asymmetrical within its own hierarchy
        # we need to resample independently each level
        # for instance USA + [2,4] means:
        # sample [2,4] among states, then sample [2,4] among cities of each state independently
        
        # clean up to keep or remove items based on valKeep/valRemove lists
        Ltmp <- getValuesFromNames(namesAndValues,valKeep,valRemove)
        
        
        
        colNames = paste(namesAndValues$names,collapse=".") 
        
        colCodes <- c(colCodes,colCode)
        colCodeNames <- c(colCodeNames,colNames)
        res <- sampleValuesFromHierarchy(Ltmp,valSample)
        DO_AGG[[colNames]]    <- res$isAggPossible
        L[[colNames]] <- res$listAttrNames 
        
        
      } else {
        # single level attribute
        Ltmp <- getValuesFromNames(namesAndValues,valKeep,valRemove)
        colNames = paste(namesAndValues$names,collapse=".") 
        
        if (isSymmetric == "false" && j>1){ # depth > 1 and asymmetric requested
          # get upper level attribute
          colCodeParent = colCodes[length(colCodes)]
          colNameParent = colCodeNames[length(colCodeNames)]
          
          # generate independent sets of attribute values for each parent attribute value
          Lnew <- NULL
          Lparent = L[[colNameParent]]
          DO_AGGnew = "true"
          for (itm in Lparent) {
            # renew sampling for each parent attribute value independently
            res = sampleValues(Ltmp,valSample)
            Lt <- res$listAttrNames
            if (res$isAggPossible == "false") DO_AGGnew = "false"
            
            Lnew = c(Lnew,paste0(itm,".",Lt))
          }
          
          # generate a composite attribute name: codeParent.codeNew
          colCodeNew = paste0(colCodeParent,".",colCode) # concatenate parent and new codes
          colCodes <- colCodes[-length(colCodes)] # remove last colCode
          colCodes <- c(colCodes,colCodeNew) # concatenate new composite code
          
          colNamesNew = paste0(colNameParent,".",colNames) # concatenate parent and new codes
          colCodeNames <- colCodeNames[-length(colCodeNames)] # remove last colCode
          colCodeNames <- c(colCodeNames,colNamesNew) # concatenate new composite code
          
          L[[colNameParent]]<- NULL  # remove upper level name
          L[[colNamesNew]] <- Lnew   # set new composite name
          DO_AGG[[colNameParent]] <- NULL
          DO_AGG[[colNamesNew]] <- DO_AGGnew
        } else {
          
          colCodes <- c(colCodes,colCode)
          colCodeNames <- c(colCodeNames,colNames)
          res = sampleValues(Ltmp,valSample)
          L[[colNames]] <- res$listAttrNames
          DO_AGG[[colNames]] <- res$isAggPossible
          
        }
      }
      
    }
    

    for (j in 1:length(rowAttr)){
      
      rowCode = unlist(rowAttr[[j]]$code)
      valKeep = unlist(rowAttr[[j]]$keep)
      valRemove = unlist(rowAttr[[j]]$remove)
      valSample = unlist(rowAttr[[j]]$sample)
      isSymmetric = unlist(rowAttr[[j]]$symmetric)
      if (length(isSymmetric)==0) isSymmetric="true"

      namesAndValues = getNamesValues(allSemanticAttributes$data,rowCode)

      depthAttr = length(namesAndValues$names)
      if (depthAttr>1){
        # multilevel attribute 
        # asymmetric/symmetric options is ignored
        # it is necessarily asymmetrical within its own hierarchy
        # we need to resample independently each level
        # for instance USA + [2,4] means:
        # sample [2,4] among states, then sample [2,4] among cities of each state independently
        
        # clean up to keep or remove items based on valKeep/valRemove lists
        Ltmp <- getValuesFromNames(namesAndValues,valKeep,valRemove)
        
        rowNames = paste(namesAndValues$names,collapse=".") 
        
        rowCodes <- c(rowCodes,rowCode)
        rowCodeNames <- c(rowCodeNames,rowNames)
        
        res <- sampleValuesFromHierarchy(Ltmp,valSample)
        DO_AGG[[rowNames]] <- res$isAggPossible
        L[[rowNames]] <- res$listAttrNames 
        
      } else {
        # single level attribute
        Ltmp <- getValuesFromNames(namesAndValues,valKeep,valRemove)
        rowNames = paste(namesAndValues$names,collapse=".") 
        
        if (isSymmetric == "false" && j>1){ # depth > 1 and asymmetric requested
          
          
          # get upper level attribute
          rowCodeParent = rowCodes[length(rowCodes)]
          rowNameParent = rowCodeNames[length(rowCodeNames)]
          
          # generate independent sets of attribute values for each parent attribute value
          Lnew <- NULL
          Lparent = L[[rowNameParent]]
          DO_AGGnew = "true"
          for (itm in Lparent) {
            # renew sampling for each parent attribute value independently
            res = sampleValues(Ltmp,valSample)
            Lt <- res$listAttrNames
            if (res$isAggPossible == "false") DO_AGGnew = "false"
            
            Lnew = c(Lnew,paste0(itm,".",Lt))
          }
          
          
          
          # generate a composite attribute name: codeParent.codeNew
          rowCodeNew = paste0(rowCodeParent,".",rowCode) # concatenate parent and new codes
          rowCodes <- rowCodes[-length(rowCodes)] # remove last rowCode
          rowCodes <- c(rowCodes,rowCodeNew) # concatenate new composite code
          
          rowNamesNew = paste0(rowNameParent,".",rowNames) # concatenate parent and new codeNames
          rowCodeNames <- rowCodeNames[-length(rowCodeNames)] # remove last rowCodeName
          rowCodeNames <- c(rowCodeNames,rowNamesNew) # concatenate new composite code
          
          
          L[[rowNameParent]]<- NULL  # remove upper level name
          L[[rowNamesNew]] <- Lnew   # set new composite name
          DO_AGG[[rowNameParent]] <- NULL
          DO_AGG[[rowNamesNew]] <- DO_AGGnew
          
          
        } else {
          rowCodes     <- c(rowCodes,rowCode)
          rowCodeNames <- c(rowCodeNames,rowNames)
          res = sampleValues(Ltmp,valSample)
          L[[rowNames]] <- res$listAttrNames
          DO_AGG[[rowNames]] <- res$isAggPossible
          
        }
      }
      
    }
    
    #####################################
    #####################################
    # generate all combinations
    #####################################
    #####################################
    expL = expand.grid(L)
    
    #####################################
    #####################################
    # get the DB table
    #####################################
    #####################################
    # split tree/composite columns
    RES <- NULL
    CNAMES <- NULL
    
    cnam = colnames(expL)
    for (cn in cnam){
      colnamcomp = unlist(strsplit(as.character(cn),split="\\."))
      lnam = length(colnamcomp)
      CNAMES <- c(CNAMES, colnamcomp)
      
      if (lnam == 1){
        if (is.null(RES)) RES <- expL[cn]
        else RES <- cbind(RES, expL[cn])
      } else {
        # split tree names (they are separated by a . like in attr1.attr2.attr3)
        ul = unlist(lapply(expL[[cn]],function(x){strsplit(as.character(x),split="\\.")}))
        RES <- cbind(RES, t(matrix(ul,lnam, length(ul)/lnam))) 
      }
    }
    
    # generate all values
    valCode = tableJSON$values  ## expect either a semantic code for values, or an array of min and max numbers [m,M]
    numSample = nrow(expL)
    semanticValues = allSemanticAttributes$values
    VAL <- getSampleValues(semanticValues,valCode,numSample,NUM_DECIMAL_DIGITS_REAL_FORMAT)
    
    if (is.integer(VAL[1])) { 
      formTot = STR_INT_VAL_FORMAT
    } else formTot = STR_REAL_VAL_FORMAT
    strValFormat = formTot
    
    # assemble the DB table
    RES <- cbind(RES,VAL)
    DBtable = data.frame(RES,stringsAsFactors = FALSE)
    colnames(DBtable) = c(CNAMES,"Value")
    DBtable$Value = as.numeric(DBtable$Value) # MAKE NUMERICAL VALUES TO PREVENT PIVOTTABLER FAILURE
    
    # update aggregation switches
    canAggTable = t(data.frame(unlist(strsplit(names(DO_AGG),split="\\."))))
    colnames(canAggTable)=unlist(strsplit(names(DO_AGG),split="\\."))
    rownames(canAggTable)="canAggregate"
    
    #### PRINT DB TABLE IN CONSOLE FOR DEBUG
    if (DO_NOT_SAVE == TRUE){ 
      print(DBtable)
    }
    
    #####################################
    #####################################
    # generate shuffle of the HCT table
    #####################################
    #####################################
    #print("SHUFFLE")
    
    NO_SHUFFLE = TRUE
    attrShuffle = tableJSON$shuffle # ["none"] / "rows" / "cols / "rowscols" / "all"
    if (length(attrShuffle)>0){
      shufflecombi = sample(length(attrShuffle))
      for (i in shufflecombi){
        curShuffleMode = attrShuffle[i]
        #print(paste0("ShuffleMode: ",curShuffleMode))
        
        shuffledColNames = colCodeNames
        shuffledRowNames = rowCodeNames
        shuffledColCodes = colCodes
        shuffledRowCodes = rowCodes
        if (attrShuffle == "cols" || attrShuffle == "rowscols") { # mix only cols
          shuffleIndex = sample(length(colCodeNames))
          shuffledColNames = colCodeNames[shuffleIndex];
          shuffledColCodes = colCodes[shuffleIndex];
          NO_SHUFFLE=FALSE
        }
        if (attrShuffle == "rows" || attrShuffle == "rowscols") { # mix only rows
          shuffleIndex = sample(length(rowCodeNames))
          shuffledRowNames = rowCodeNames[shuffleIndex];
          shuffledRowCodes = rowCodes[shuffleIndex];
          NO_SHUFFLE=FALSE
        }
        if (attrShuffle == "all") { # mix rows and columns codes indistinctly
          rowcolCodeNames = c(rowCodeNames,colCodeNames)
          rowcolCodes = c(rowCodes,colCodes)
          shuffleIndex = sample(length(rowcolCodeNames))
          shuffledRowsColsNames = rowcolCodeNames[shuffleIndex]
          shuffledRowsColsCodes = rowcolCodes[shuffleIndex]
          shuffledRowNames = shuffledRowsColsNames[1:length(rowCodeNames)]
          shuffledColNames = shuffledRowsColsNames[(length(rowCodeNames)+1):length(shuffledRowsColsNames)]
          shuffledRowCodes = shuffledRowsColsCodes[1:length(rowCodes)]
          shuffledColCodes = shuffledRowsColsCodes[(length(rowCodes)+1):length(shuffledRowsColsCodes)]
          NO_SHUFFLE=FALSE
        }
      }
      
      colCodeNames = unlist(lapply(shuffledColNames,function(x){strsplit(as.character(x),split="\\.")}))
      rowCodeNames = unlist(lapply(shuffledRowNames,function(x){strsplit(as.character(x),split="\\.")}))
      colCodes = unlist(lapply(shuffledColCodes,function(x){strsplit(as.character(x),split="\\.")}))
      rowCodes = unlist(lapply(shuffledRowCodes,function(x){strsplit(as.character(x),split="\\.")}))
      
      
    } else {
      NO_SHUFFLE=TRUE
      
      colCodeNames = unlist(lapply(colCodeNames,function(x){strsplit(as.character(x),split="\\.")}))
      rowCodeNames = unlist(lapply(rowCodeNames,function(x){strsplit(as.character(x),split="\\.")}))
      
      colCodes = unlist(lapply(colCodes,function(x){strsplit(as.character(x),split="\\.")}))
      rowCodes = unlist(lapply(rowCodes,function(x){strsplit(as.character(x),split="\\.")}))
    }
    
    
    
    ###################################################
    ###################################################
    # STYLING OF PIVOT TABLE
    ###################################################
    ###################################################
    #print("Styling")
    
    #### RANDOM STYLING
    addBorderLines = sample(c(FALSE,TRUE),1) # TRUE/FALSE 50%
    
    ###################################################
    ###################################################
    # generate and check signature to avoid same table
    ###################################################
    ###################################################
    #print("Check signature")
    
    border = "WithoutBorderLines"
    if (addBorderLines) border="WithBorderLines"
    
    allColsRowsValuesSTR = paste0(unlist(L),collapse = '&&&')# make list as strings
    colCodeNamesSTR = paste0(colCodeNames,collapse = '&&&') # make list as strings
    rowCodeNamesSTR = paste0(rowCodeNames,collapse = '&&&') # make list as strings
    
    curSignatureDB =  paste0("ALL&&&",allColsRowsValuesSTR,"&&&COLS&&&", colCodeNamesSTR,"&&&ROWS&&&", rowCodeNamesSTR, "&&&STYLE&&&", border)
    signatureAGGempty = paste0("&&&AGG_NAME&&&","",
                          "&&&AGG_FUN&&&","",
                          "&&&AGG_COLS&&&","",
                          "&&&AGG_ROWS&&&","")
    curSignature = paste0(curSignatureDB,signatureAGGempty)
    
    #print(paste0("Signature: ",curSignature))
    
    if (curSignature %in% SIGNATURES){
      print(paste0("------------------------------------------- WARNING --- Table design already exist, trial :",trialCnt,"/",(trialCntMAX-1)))
      
      # already exist
      # resample row/col/style
      trialCnt = trialCnt + 1
      if (trialCnt >= trialCntMAX) {
        if (curReplica == numReplica) geneNextReplica = FALSE
        trialCnt = 0
      } else {
        curReplica = curReplica-1
        geneNextReplica = TRUE
      }
    } else { 
      print(paste0("Table design is unique, proceed..."))
      
      SIGNATURES <- c(SIGNATURES, curSignature)
      
      
      
      if (DO_NOT_SAVE == FALSE){ 
        #####################################
        #####################################
        # save DB table
        #####################################
        #####################################
        
        # save the DB table
        
        print("Generate CSV for DB TABLE")
        
        # get clean attribute names for DB SQL query
        colnamesSQL = getSQLattrNames(colnames(DBtable))
        # add the SQL-compatible attribute names as second row in the csv file (keep human-readable names as first row)
        DBtableCleanAsSTRING = rbind(colnamesSQL, sapply(DBtable, as.character))
        filenameCSV = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_DB.csv")
        write.csv(DBtableCleanAsSTRING,file = filenameCSV,row.names = FALSE, quote=FALSE)
        
        if (DO_NOT_SAVE_DB_HTML == FALSE){
          print("Generate HTML for DB TABLE")
          # HTML assumed for human consumption, we do not add the SQL-compatible attribute names
          filenameHTML = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_DB.html")
          unlink(filenameHTML)
          sink(filenameHTML)
          print(htmlTable(DBtable),type="html",useViewer = FALSE)
          sink()
        }
        
        ########################## SAVE JSON SIGNATURE ##############################
        jsonSTR_SIG_DB = paste(c("{'id': '",paste0(outputNamePrefix,"_",curReplica),"',",
                                    "'formatValue': '",strValFormat, "',",
                                    "'seedValue': '",seedValue, "',",
                                    "'signature': '",curSignature, "'}"),sep="", collapse="")
        ########################## PRINT JSON ##############################
        print("Generate JSON signature (no aggregation info)")
        filenameJSON_SIG_DB = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_SIG_DB.json")
        
        # clean json data to use only -"- as quotes 
        jsonSTR_SIG_DB=gsub("'",'"' ,jsonSTR_SIG_DB)
        
        write.table(jsonSTR_SIG_DB, file = filenameJSON_SIG_DB, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
        
        
      } # DO_NOT_SAVE
      
      
      #####################################
      #####################################
      # check symmetry from generated DB table 
      # (not from the requested table template which may have failed), 
      # for JSON table format
      #####################################
      #####################################
      rowNestingSymmetric = 'true'
      multiLevelColumnSymmetric = 'true'
      
      # get fields of L which contain aggregated names (.)
      for (iname in names(L)){
        ulName = unlist(strsplit(iname,"\\."))
        if (length(ulName)>1) {
          # composite name, unfold to check symmetry
          uL = unlist(strsplit(L[[iname]],"\\."))
          luL = length(uL)
          lL = length(L[[iname]])
          nk = luL / lL
          mL = matrix(uL,nk,lL)
          for (ir in 2:nrow(mL)) {
            if (length(unique(table(mL[ir,])))>1) {
              # asymmetric values
              if (ulName[1] %in% colCodeNames) { 
                multiLevelColumnSymmetric = "false" 
              } else if (ulName[1] %in% rowCodeNames) rowNestingSymmetric = "false"
            }
          }
        }
      }
      
      if (DO_NOT_GENERATE_PIVOT == FALSE){
        #####################################
        #####################################
        # generate the pivot table
        #####################################
        #####################################
        print("Generate PIVOT TABLE")
        
        #### COL PRESET for JSON table format
        cumColAddTot = 0
        multiLevelColumn = "false"
        columnAggregationLocal = "false"
        columnAggregationGlobal = "false"
        aggColAttrNames = NULL
        
        #### ROW PRESET for JSON table format
        cumRowAddTot = 0
        rowNesting = "false"
        rowAggregationLocal = "false"
        rowAggregationGlobal = "false"
        rowGroupLabel = "false"
        aggRowAttrNames = NULL
        
        pt <- PivotTable$new()
        pt$addData(DBtable) 
        
        ### replace default "Total" by agg_name
        pt$setDefault(totalCaption = agg_name,  
                      totalPosition = "after") # "before" or "after"
        
        
        if (NO_SHUFFLE == TRUE){
          for (clc in colCodes){
            namesAndValues = getNamesValues(allSemanticAttributes$data,clc)
            nam = unlist(namesAndValues$names)
            for (nm in nam) {
              indAttr = getAttrFromCodes(colAttr,clc)
              if (colAttr[[indAttr]]$agg_pos1 != "none" ) {
                addTot = TRUE
              } else addTot = FALSE
              
              #### Check if aggregation is still possible given the random sampled set of attribute values
              if (canAggTable[,nm] == "false") addTot = FALSE
              
              #### for JSON table format
              if (addTot) {
                cumColAddTot = cumColAddTot + 1
                aggColAttrNames = c(aggColAttrNames,nm)
              }
              
              pt$addColumnDataGroups(nm,addTotal=addTot) 
            }
          }
          #### UPDATE for JSON table format
          if (length(colCodeNames)>1) multiLevelColumn = "true" 
          if (cumColAddTot == length(colCodeNames)) columnAggregationGlobal = "true"
          if (cumColAddTot > 1) columnAggregationLocal = "true"
          
          row_level = 0
          indentRowWithTotal = FALSE
          for (rwc in rowCodes){
            namesAndValues = getNamesValues(allSemanticAttributes$data,rwc)
            nam = unlist(namesAndValues$names)
            for (nm in nam) {
              row_level = row_level + 1
              indAttr = getAttrFromCodes(rowAttr,rwc)
              
              if (rowAttr[[indAttr]]$agg_pos1 != "none" ) {
                addTot = TRUE
              } else addTot = FALSE
              
              #### Check if aggregation is still possible given the random sampled set of attribute values
              if (canAggTable[,nm] == "false") addTot = FALSE
              
              #### for JSON table format
              if (addTot) {
                cumRowAddTot = cumRowAddTot + 1
                aggRowAttrNames = c(aggRowAttrNames,nm)
              }
              
              # create empty row only if at row level 1 and row_format is not "new" ("new" meaning set a new column for the next level, so no indent),
              if (length(rowTableFormat)>0 && rowTableFormat != "new" && row_level == 1) {
                indentRow = TRUE # create empty row heading row level 1
              } else indentRow = FALSE
              
              print(paste0(nm, " indentRow  ", indentRow))
              
              ### format the indented row
              if (indentRow == TRUE && addTot == TRUE){
                if (rowAttr[[indAttr]]$agg_pos1 == "top") {
                  outlineBefore = list(isEmpty=FALSE, mergeSpace="dataGroupsOnly")
                  indentRowWithTotal = TRUE 
                }else {
                  outlineBefore = list(isEmpty=TRUE, mergeSpace="dataGroupsAndCellsAs1")
                  #### for JSON table format
                  rowGroupLabel = "true"
                }
              } else if (indentRow == TRUE && addTot == FALSE) {
                outlineBefore = TRUE
                #### for JSON table format
                rowGroupLabel = "true"
              } else outlineBefore = FALSE
              
              # do not add the total at level 2 if it has been done already
              if (row_level > 1 && indentRowWithTotal == TRUE) addTot = FALSE 
              if (row_level > 1 ) outlineBefore = FALSE 
              
              pt$addRowDataGroups(nm,outlineBefore=outlineBefore, addTotal=addTot) 
            }
          }
          #### for JSON table format
          if (length(rowCodeNames)>1) rowNesting = "true" 
          if (cumRowAddTot == length(rowCodeNames)) rowAggregationGlobal = "true"
          if (cumRowAddTot > 1) rowAggregationLocal = "true"
          
        } else { ### SHUFFLE HAPPENED
          #print("SHUFFLE HAPPENED")
          
          #### GENERATE PIVOT COLUMNS
          for (nm in colCodeNames) {
            cdc = getCodesFromName(allSemanticAttributes,nm)
            indAttrR = getAttrFromCodes(rowAttr,cdc) 
            indAttrC = getAttrFromCodes(colAttr,cdc)
            if (length(indAttrR)>0) {
              rowcolAttr = rowAttr[[indAttrR]]
            } else rowcolAttr = colAttr[[indAttrC]]
            
            if (rowcolAttr$agg_pos1 != "none" ) {
              addTot = TRUE
            } else addTot = FALSE
            
            #### Check if aggregation is still possible given the random sampled set of attribute values
            if (canAggTable[,nm] == "false") addTot = FALSE
            
            #### for JSON table format
            if (addTot) {
              cumColAddTot = cumColAddTot + 1
              aggColAttrNames = c(aggColAttrNames,nm)
            }
            
            pt$addColumnDataGroups(nm,addTotal=addTot) 
          }
          #### UPDATE for JSON table format
          if (length(colCodeNames)>1) multiLevelColumn = "true" 
          if (cumColAddTot == length(colCodeNames)) columnAggregationGlobal = "true"
          if (cumColAddTot > 1) columnAggregationLocal = "true"
          
          
          
          #### GENERATE PIVOT ROWS
          row_level = 0
          indentRowWithTotal = FALSE
          for (nm in rowCodeNames) {
            row_level = row_level + 1
            rwc = getCodesFromName(allSemanticAttributes,nm)
            indAttrR = getAttrFromCodes(rowAttr,rwc) 
            indAttrC = getAttrFromCodes(colAttr,rwc)
            if (length(indAttrR)>0) { 
              rowcolAttr = rowAttr[[indAttrR]]
            } else rowcolAttr = colAttr[[indAttrC]]
            
            if (rowcolAttr$agg_pos1 != "none" ) {
              addTot = TRUE
            } else addTot = FALSE
            
            #### Check if aggregation is still possible given the random sampled set of attribute values
            if (canAggTable[,nm] == "false") addTot = FALSE
            
            #### for JSON table format
            if (addTot) {
              cumRowAddTot = cumRowAddTot + 1
              aggRowAttrNames = c(aggRowAttrNames,nm)
            }
            
            # create empty row only if at row level 1 and row_format is not "new" ("new" meaning set a new column for the next level, so no indent),
            if (length(rowTableFormat)>0 && rowTableFormat != "new" && row_level == 1) {
              indentRow = TRUE # create empty row heading row level 1
            } else indentRow = FALSE
            
            
            
            ### format the indented row
            if (indentRow == TRUE && addTot == TRUE){
              if (rowcolAttr$agg_pos1 == "top") {
                outlineBefore = list(isEmpty=FALSE, mergeSpace="dataGroupsOnly")
                indentRowWithTotal = TRUE 
              }else {
                outlineBefore = list(isEmpty=TRUE, mergeSpace="dataGroupsAndCellsAs1")
                #### for JSON table format
                rowGroupLabel = "true"
              }
            } else if (indentRow == TRUE && addTot == FALSE) {
              outlineBefore = TRUE
              #### for JSON table format
              rowGroupLabel = "true"
            } else outlineBefore = FALSE
            
            # do not add the total at level 2 if it has been done already
            if (row_level > 1 && indentRowWithTotal == TRUE) addTot = FALSE 
            if (row_level > 1 ) outlineBefore = FALSE 
            
            pt$addRowDataGroups(nm,outlineBefore=outlineBefore, addTotal=addTot) 
          }
          #### for JSON table format
          if (length(rowCodeNames)>1) rowNesting = "true" 
          if (cumRowAddTot == length(rowCodeNames)) rowAggregationGlobal = "true"
          if (cumRowAddTot > 1) rowAggregationLocal = "true"
          
        }
        
        ### update the signature
        signatureAGG = paste0("&&&AGG_NAME&&&",agg_name,
                              "&&&AGG_FUN&&&",agg_fun,
                              "&&&AGG_COLS&&&",paste0(aggColAttrNames,collapse="&&&"),
                              "&&&AGG_ROWS&&&",paste0(aggRowAttrNames,collapse="&&&"))
        signatureHCT = paste0(curSignatureDB,signatureAGG)
        
        #### RECORDING AGGREGATING FUNCTION avg, min, max, (default is sum)
        if (agg_fun == "avg"){ ### USE "avg" instead of "mean" as standard in SQL
          pt$defineCalculation(calculationName=agg_name, caption = agg_name,
                               summariseExpression="mean(Value,na.rm = TRUE)",
                               format= STR_REAL_VAL_FORMAT)
          strValFormat = STR_REAL_VAL_FORMAT
        } else if (agg_fun == "min") {
          pt$defineCalculation(calculationName=agg_name, caption = agg_name,
                               summariseExpression="min(Value,na.rm = TRUE)",
                               format= STR_REAL_VAL_FORMAT)
          strValFormat = STR_REAL_VAL_FORMAT
        } else if (agg_fun == "max") {
          pt$defineCalculation(calculationName=agg_name, caption = agg_name,
                               summariseExpression="max(Value,na.rm = TRUE)",
                               format= STR_REAL_VAL_FORMAT)
          strValFormat = STR_REAL_VAL_FORMAT
        } else {  ### DEFAULT is tot/sum, aggregation function always needed, otherwise generate empty pivot
          pt$defineCalculation(calculationName=agg_name, caption = agg_name,
                               summariseExpression="sum(Value)",
                               format=formTot)
          strValFormat = formTot
        }
        
        
        pt$evaluatePivot()
        
        # data.frame of the pivoted table
        dfPivot <- pt$asDataFrame()
        
        # check if any NA values (can happen if too few columns or rows when aggregating with mean)
        doNotSave = FALSE
        warningMsg = ""
        if (any(is.na(dfPivot))) warningMsg = paste0(warningMsg," Contains NA! ")
        if (ncol(dfPivot)<2) warningMsg = paste0(warningMsg," Only one column! ")
        if (nrow(dfPivot)<2) warningMsg = paste0(warningMsg," Only one row! ")
        
        if (nchar(warningMsg)>0) {
          DO_NOT_SAVE = TRUE
          print(paste0("***** WARNING! ***** ",warningMsg))
        } else DO_NOT_SAVE = FALSE
        
        
        # SAVE generated pivot
        totalNumTableGene = totalNumTableGene + 1 # counter for all tables generated
        
        print(paste0("************** SAVING TABLE ************* ",totalNumTableGene))
        
        allPivot$warningMsg[[totalNumTableGene]] = warningMsg
        allPivot$df[[totalNumTableGene]]= dfPivot
        allPivot$signature[[totalNumTableGene]] = signatureHCT
        
        if (DO_NOT_SAVE == FALSE){ 
          #################################################################################
          ############## PREPARE HTML OF PIVOT TABLE FOR AUTO-RESCALE #####################
          #################################################################################
          print("Generate HTML for PIVOT TABLE")
          
          if (HTML_OK){
            filenameHTML = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_HCT.html")
            unlink(filenameHTML)
            
            if (addBorderLines) {
              styleSTR = "<style>
            Table {
            border: 1px solid; 
            border-collapse: collapse;
            }
            th, td {
            border: 1px solid;
            padding: 5px;
            text-align: center;
            vertical-align: center;
            }
            </style>" 
            } else {
              styleSTR = "<style>
            Table {
            border: 0px solid; 
            border-collapse: collapse;
            }
            th, td {
            border: 0px solid;
            padding: 5px;
            text-align: left;
            vertical-align: top;
            }
            </style>" 
            }
            
            HTMLtableHEAD = paste0("<html><head>",styleSTR,"</head><body><div class='mytable'>")
            HTMLtableTITLE = paste("<h1>",tableTitle,"</h1>")
            HTMLtableCONTENT = paste(pt$getHtml(), sep="", collapse="\n")
            HTMLtableTAIL = " </div><script>
          // AUTOSCALING (fontsize determines cell dimensions, and so page width/height)
          let tb = document.getElementsByClassName('Table');
          let pr = document.getElementsByClassName('mytable');
          
          let cw = pr[0].clientWidth
          let sw = pr[0].scrollWidth
          let ch = pr[0].clientHeight
          let sh = window.innerHeight
          
          for (i=1;i<100;i++){
          let scale = (cw/sw) 
          tb[0].style.fontSize = (i*scale) + 'px'   
          cw = pr[0].clientWidth
          sw = pr[0].scrollWidth
          ch = pr[0].clientHeight
          sh = window.innerHeight 
          
          if ((cw < sw)||(ch > sh)) { 
          let cnt = i-1
          if (cnt == 0) cnt = 1
          tb[0].style.fontSize = (cnt*scale) + 'px'
          break
          }
          }
          </script></body>"
            
            
            HTMLtableSTR = paste0(HTMLtableHEAD,HTMLtableTITLE,HTMLtableCONTENT,HTMLtableTAIL)
            write(HTMLtableSTR,file = filenameHTML, append = FALSE)
          }
          
          ########################## PRINT PDF FROM HTML ##############################
          filenamePDF = filenameHTML
          if (PDF_OK){print("Generate PDF of PIVOT TABLE")
            filenamePDF = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_HCT.pdf")
            chrome_print(input = filenameHTML,output = filenamePDF,
                      options = list("landscape" = TRUE,"printBackground" = FALSE,  "preferCSSPageSize" = FALSE))
          }

          
          if (PNG_OK && PDF_OK){
            print("Generate PNG of PIVOT TABLE")
            filenamePNG = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_HCT.png")
            magick::image_read_pdf(filenamePDF) %>% image_write(., path = filenamePNG, format = "png")
          }
            
          ########################## PRINT CSV ##############################
          print("Generate CSV of PIVOT TABLE")
          filenameCSV = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_HCT.csv")
          mat = pt$asMatrix()
          write.table(mat,file = filenameCSV,row.names = FALSE, col.names= FALSE,sep=",")
          
          #####################################
          #####################################
          # generate JSON table format
          #####################################
          #####################################
          rowAggregationGlobal_OR_Local = "false"
          if (rowAggregationGlobal == "true" || rowAggregationLocal == "true") rowAggregationGlobal_OR_Local = "true"
          columnAggregationGlobal_OR_Local = "false"
          if (columnAggregationGlobal == "true" || columnAggregationLocal == "true") columnAggregationGlobal_OR_Local = "true"
          
          NOT_multiLevelColumnSymmetric ="false"
          if (multiLevelColumnSymmetric == "false") NOT_multiLevelColumnSymmetric = "true"
          NOT_rowNestingSymmetric = "false"
          if (rowNestingSymmetric == "false") NOT_rowNestingSymmetric = "true"
          
          ########################## SAVE JSON SIGNATURE ##############################
          jsonSTR_SIG_HCT = paste(c("{'id': '",paste0(outputNamePrefix,"_",curReplica),"',",
                                   "'formatValue': '",strValFormat, "',",
                                   "'seedValue': '",seedValue, "',",
                                   "'signature': '",signatureHCT, "'}"),sep="", collapse="")
          ########################## PRINT JSON ##############################
          print("Generate JSON signature  for Query generation (with full HCT aggregation info)")
          filenameJSON_SIG_HCT = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_SIG_HCT.json")
          
          # clean json data to use only -"- as quotes 
          jsonSTR_SIG_HCT=gsub("'",'"' ,jsonSTR_SIG_HCT)
          
          write.table(jsonSTR_SIG_HCT, file = filenameJSON_SIG_HCT, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
          
          ########################## SAVE JSON ##############################
          jsonSTR = paste(c("{'id': '",paste0(outputNamePrefix,"_",curReplica),"',",
                            "'formatValue': '",strValFormat, "',",
                            "'seedValue': '",seedValue, "',",
                            "'signature': '",signatureHCT, "',",
                            "'image_source': '", filenamePDF,"',",  
                            "'state': 'labelled',",
                            " 'concern': false,",
                            " 'notes': '',",
                            " 'properties': {",
                            " 'Standard Relational Table': false,",
                            " 'Multi Level Column': ", multiLevelColumn,",",
                            " 'Balanced Multi Level Column': true,",
                            " 'Symmetric Multi Level Column': ", multiLevelColumnSymmetric,",",
                            " 'Unbalanced Multi Level Column': false,",
                            " 'Asymmetric Multi Level Column': ", NOT_multiLevelColumnSymmetric,",",
                            " 'Column Aggregation': ", columnAggregationGlobal_OR_Local,",",
                            " 'Global Column Aggregation':",columnAggregationGlobal,",",
                            " 'Local Column-Group Aggregation':", columnAggregationLocal,",",
                            " 'Explicit Column Aggregation Terms': true,",
                            " 'Implicit Column Aggregation Terms': false,",
                            " 'Row Nesting': ", rowNesting,",",
                            " 'Balanced Row Nesting': true,",
                            " 'Symmetric Row Nesting': ",rowNestingSymmetric,",",
                            " 'Unbalanced Row Nesting': false,",
                            " 'Asymmetric Row Nesting': ", NOT_rowNestingSymmetric,",",
                            " 'Row Aggregation': ",rowAggregationGlobal_OR_Local,",",
                            " 'Global Row Aggregation': ",rowAggregationGlobal,",",
                            " 'Local Row-Group Aggregation': ",rowAggregationLocal,",",
                            " 'Explicit Row Aggregation Terms': true,",
                            " 'Implicit Row Aggregation Terms': false,",
                            " 'Split Header Cell': false,",
                            " 'Row Group Label': ",rowGroupLabel,"},",
                            " 'themes': []}"),sep="", collapse="")
          
          ########################## PRINT JSON ##############################
          print("Generate JSON for PIVOT TABLE")
          filenameJSON = paste0(outputFolder,outputNamePrefix,"_",curReplica,"_HCT.json")
          
          # clean json data to use only -"- as quotes 
          jsonSTR=gsub("'",'"' ,jsonSTR)
          
          write.table(jsonSTR, file = filenameJSON, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
          
        } # DO_NOT_SAVE
        
        
      } # DO_NOT_GENERATE_PIVOT
      
      #save(allPivot,file="allPivotTables.RData")
      
    } # unique design, generation can proceed
    
  } # end loop on replica of current table
  
} # end loop on tables


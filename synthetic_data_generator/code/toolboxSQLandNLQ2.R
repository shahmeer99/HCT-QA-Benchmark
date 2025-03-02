################ I/O #####################
# get all row headers of HCT table (without aggregate rows nor DB row colnames)
getHCTrowHeaders <- function(DBdata,colNames,rowNames){
  colVal <- DBdata[1,colNames,drop=FALSE]
  filtRows <- DBdata
  for (i in 1:length(colNames)){
    filtRows = filtRows[which(filtRows[,colNames[i]] == unlist(colVal[i])),]
  }
  return(filtRows[,rowNames,drop=FALSE])
}

# get all col headers of HCT table (without aggregate cols nor DB col colnames)
getHCTcolHeaders <- function(DBdata,colNames,rowNames){
  rowVal <- DBdata[1,rowNames,drop=FALSE]
  filtCols <- DBdata
  for (i in 1:length(rowNames)){
    filtCols = filtCols[which(filtCols[,rowNames[i]] == unlist(rowVal[i])),]
  }
  return(filtCols[,colNames,drop=FALSE])
}


# HCT COLUMNS or ROWS (multi)-selection at random
# run geneIndexForRandomClauses(HCTrowHeaders,1,1) for selecting a single HCT row
# run geneIndexForRandomClauses(HCTcolHeaders,1,1) for selecting a single HCT column
# run geneIndexForRandomClauses(HCTrowHeaders,mn,mx) for selecting between mn and mx HCT rows
# run geneIndexForRandomClauses(HCTcolHeaders,mn,mx) for selecting between mn and mx HCT cols
# run geneIndexForRandomClauses(HCTrowHeaders) or ...,0,0) for selecting all HCT rows (default)
# run geneIndexForRandomClauses(HCTcolHeaders) or ...,0,0) for selecting all HCT columns (default)
geneIndexForRandomClauses <- function(HCTheaders,minLevVal=0, maxLevVal=0){
  nrowHCT = nrow(HCTheaders)
  if (minLevVal==0 && maxLevVal == 0) { # select all col/row without sampling
    nColSel = nrowHCT
    indColSel = 1:nrowHCT
  } else {
    
    if (minLevVal == maxLevVal){
      nColSel = minLevVal
    } else {
      nColSel = sample(max(1,min(minLevVal,maxLevVal)):min(nrowHCT,max(minLevVal,maxLevVal)))[1] 
    }
    indColSel = sort(sample(1:nrowHCT)[1:nColSel])
  }
  return(indColSel)
}

# GENERATE ROW/COL SELECTION SQL CLAUSE 
# from list of ROW or COLUMN HEADER dataframe and row index
# call indColSel <- geneIndexForRandomClauses(HCTheaders,minLevVal, maxLevVal)
# then call rowColClause = geneMultiRowOrColSelectionSQL(HCTheaders,indColSel)
geneMultiRowOrColSelectionSQL <- function(HCTheaders,indColSel){
  # generate OR or IN clauses
  HCTcol_attrNames = colnames(HCTheaders)
  ncolHCT = ncol(HCTheaders)
  
  nColSel = length(indColSel)
  if (ncolHCT>1){
    # generate the OR clause
    ncolHCT = ncol(HCTheaders)
    colClause = ""
    for (ic in 1:nColSel){
      indCol = indColSel[ic]
      rowClauseLIST <- NULL
      irk = 0
      for (ir in 1:ncolHCT){
        valRowAttr = HCTheaders[indCol,ir] 
        if (!is.na(valRowAttr)){
          irk = irk + 1
          rowClauseLIST[[irk]] = paste0(HCTcol_attrNames[ir]," = '",valRowAttr,"'")
        }
      }
      rowClauseTMP = paste0(rowClauseLIST,collapse=" AND ")
      rowClauseTMP = paste0("(",rowClauseTMP,")")
      
      colClause = paste0(colClause,rowClauseTMP)
      if (ic < nColSel) colClause = paste0(colClause," OR ")
    }
  } else {
    # generate the IN clause
    ick = 0
    colClauseLIST <- NULL
    for (ic in 1:nColSel){
      indCol = indColSel[ic]
      valRowAttr = HCTheaders[indCol,1] 
      if (!is.na(valRowAttr)){
        ick = ick + 1
        colClauseLIST[[ick]] = paste0("'",valRowAttr,"'")
      }
    }
    colClauseTMP = paste0(colClauseLIST,collapse=",")
    colClause = paste0(HCTcol_attrNames," IN (",colClauseTMP,")")
  }
  colClause = paste0("(",colClause,")")
  return(colClause)
}

# Generate all columns at a certain level for which the aggregate is displayed in HCT
geneAggregatedSelectionSQL <- function(HCTcolHeaders,aggColToGen){
  # code commented for columns, applies to rows the same way 
  # by using HCTrowHeaders and aggRowAttrNames
  HCTcol_attrNames = colnames(HCTcolHeaders)
  # pick one value for any of the other column attributes
  indLevelAgg = which(HCTcol_attrNames==aggColToGen)
  if (indLevelAgg>1) { 
    otherCols = HCTcol_attrNames[1:(indLevelAgg-1)]
  } else {
    otherCols <- NULL
  }
  
  # if cname2 = aggColToGen (attribute name at level 2)
  # generate clause of the form: "cname1 = A AND cname3 = C AND (cname2 IN (B1,B2,...Bk)) 
  colClause = NULL
  strFilter = NULL
  if (!is.null(otherCols)){
    if (length(otherCols) == 1){
      ucol = unique(HCTcolHeaders[[otherCols]])
      uucol = sample(ucol)[1]
      colClause = c(colClause,paste0(otherCols," = '",uucol,"'"))
      strFilter = c(strFilter,paste0("HCTcolHeaders[['",otherCols,"']] == '",uucol,"'"))
      
      otherColNames = otherCols
      otherColValues = uucol
      
    }else{ # need to look only at possible values having these upper level columns
      ucol = HCTcolHeaders[,otherCols] %>% dplyr::distinct()
      iucol = sample(nrow(ucol))[1]
      uucol = ucol[iucol,,drop=FALSE]
      
      for (cl in colnames(uucol)){
        colClause = c(colClause,paste0(cl," = '",uucol[[cl]],"'"))
        strFilter = c(strFilter,paste0("HCTcolHeaders[['",cl,"']] == '",uucol[[cl]],"'"))
      }
      otherColNames = colnames(uucol)
      otherColValues = uucol
      
    }
  }else{
    otherColNames = ""
    otherColValues = ""
    
  }
  
  # add the list of values of the aggregated col attribute
  if (!is.null(strFilter)) {
    listVal = unique(HCTcolHeaders[eval(parse(text=paste0(strFilter,collapse = " & "))),aggColToGen])
  }else { 
    listVal = unique(HCTcolHeaders[[aggColToGen]])
  }
  ucol = paste0(listVal,collapse = "','")
  
  colClause = c(colClause,paste0("(",aggColToGen," IN ('",ucol,"'))"))
  
  geneColNames = aggColToGen
  geneColValues = listVal
  
  res <- NULL
  res$otherColNames = otherColNames
  res$otherColValues = otherColValues
  res$geneColNames = geneColNames
  res$geneColValues = geneColValues
  res$colClause = paste(colClause,collapse=" AND ")
  return(res)
}

# generate an expression clause on the output value
geneExpressionClause <- function(expression="",valAttrName = "Value"){
  exprClause <- NULL
  for (ixp in 1:length(expression)) {
    expr = expression[ixp]
    if (nchar(expr)!=0){
      curExprClause = paste0(expr,"(",valAttrName,")")  # "avg(Value)" "min(Value)"
      exprClause = c(exprClause, curExprClause);
    }
  }
  exprClause = paste(exprClause,collapse=",")
  return(exprClause)
}

# generate a GROUP BY attribute randomly among the attributes which have different values in the colOrRowClause
# call geneGroupByClause(HCTcolHeaders,colClause) for a grouping per columns
# call geneGroupByClause(HCTrowHeaders,rowClause) for a grouping per rows
# colOrRowClause is like: 
#"((Year = '2016/2017' AND Nationality = 'Non-Qatari' AND Quarter = 'Q3') OR (Year = '2017/2018' AND Nationality = 'Qatari' AND Quarter = 'Q4'))"
geneGroupByClause <- function(HCTcolOrRowHeaders,colOrRowClause){
  candidates <- NULL
  for (cn in colnames(HCTcolOrRowHeaders)){
    # check if it varies across colOrRowClause
    strtmp = unlist(strsplit(colOrRowClause," "))
    indcn = which(grepl(cn, strtmp, fixed = TRUE))
    if (length(unique(strtmp[indcn+2]))>1) candidates = c(candidates,cn)
  }
  if (!is.null(candidates)) {
    return(sample(candidates)[1])
  } else {
    return("")
  }
}  


sqlJSONtemplate <- function(tempNum,sqlClause,sqlResultSTR){
  # empty spaces are on purpose to indent and ease reading of the resulting JSON file
  return(paste0('{"name": "template ',tempNum,'",\n "sql": "',sqlClause,'",\n "sqlResult": "',sqlResultSTR,'"}')) 
}

sqlnlqJSONtemplate <- function(tempNum,sqlClause,sqlResultSTR,NLquestionVariants){
  
  # empty spaces are on purpose to indent and ease reading of the resulting JSON file
  
  if (length(NLquestionVariants) == 1){
    return(paste0('{"name": "template ',tempNum,'",\n "sql": "',sqlClause,'",\n "sqlResult": "',sqlResultSTR,'",\n "nlq": "',NLquestionVariants[1],'"}')) 
  } else {
    strStart = paste0('{"name": "template ',tempNum,'",\n "sql": "',sqlClause,'",\n "sqlResult": "',sqlResultSTR,'",\n "nlq": [')
    strCur = paste0('"',NLquestionVariants[1],'"') 
    for (i in 2:length(NLquestionVariants)){
       strCur = paste0(strCur, ',\n         "',NLquestionVariants[i],'"') 
    } 
    strEnd = ']}'
    return(paste0(strStart,strCur,strEnd))
  }
}

# get all unique attribute values at a certain level of a dataframe of attributes
getUniqueValuesAtLevel <- function(HCTheaders,valLevel){
  HCTatLevel = as.data.frame(unique(HCTheaders[,valLevel]))
  colnames(HCTatLevel)=colnames(HCTheaders)[valLevel]
  return(HCTatLevel)
}

formatResults <- function(sqlResult){
  # {A} || {B} || {C} = 1 column, 3 rows
  # {A | B | C} = 1 row, 3 columns
  # {A | B | C} || {D | E | F} = 2 rows, 3 columns
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  if (nrow(sqlResult)==0) return(NULL)
  print("++++++++++++++++ formatResults +++++++++++++++")
  print(sqlResult)
  print(nrow(sqlResult))
  print(ncol(sqlResult))
  if (nrow(sqlResult)==1) {
    for (ic in 1:ncol(sqlResult)){
      sr = sqlResult[1,ic]
      print(sr)
      print(is.numeric(sr))
      if (is.numeric(sr)) sqlResult[1,ic] = round(sr, digits = NUM_DECIMAL_DIGITS_REAL_FORMAT)
    }
    print(sqlResult)
    
    return(paste0("{",paste0(as.character(sqlResult),collapse = COL_SEP), "}"))
  }
  sqlResultSTR<-NULL
  for (ir in 1:nrow(sqlResult)){
    for (ic in 1:ncol(sqlResult)){
      sr = sqlResult[ir,ic]
      if (is.numeric(sr)) sqlResult[ir,ic] = round(sr, digits = NUM_DECIMAL_DIGITS_REAL_FORMAT)
    }
    
    sqlResultSTR = c(sqlResultSTR, paste0("{",paste0(as.character(sqlResult[ir,]),collapse = COL_SEP), "}"))
  }
  sqlResultSTR = paste(sqlResultSTR,collapse = ROW_SEP)
  
  return(sqlResultSTR)
}

############################## NLQ ################################

# "of_$Gender_students" --> "Gender"
# "of_$Type_of_pollution_pollution" --> Type_of_pollution
# "in_$Language" --> "Language"
# "$Language" --> Language
getAttrNamesFromPattern <- function(nlPatternSegment,listAttrNames){
  
  varName2=NULL
  for (curAttrName in listAttrNames) {
    if (grepl(paste0("\\$",curAttrName),nlPatternSegment)){
      varName2 = c(varName2,curAttrName)
    }
  }
  
  return(varName2)
}
  
# get the attribute names (Nationality, Gender) from strPattern ("the_$Nationality_$Gender_students) 
# which are actually in that class of table (listed in allNLattrNames) 
# but not necessarily in the current table instance: e.g.: "the Qatari students" or "the Male students"
getValidAttrNames <- function(strPattern,allNLattrNames){
  
  # get all attributes ($xxx) in strPattern, add "_" as an attribute "xx zzz" is stored as HCThead$xx_zzz
  allSQLattrNames = getSQLattrNames(allNLattrNames)
  ftInGroup = NULL
  for (i in 1:length(allSQLattrNames)){
    ftname = allSQLattrNames[i]
    splitSTR = unlist(strsplit(strPattern,split=paste0("\\$",ftname)))
    if (splitSTR[1] != strPattern) # there is a split
      ftInGroup = c(ftInGroup,ftname) 
  }
  return(ftInGroup)
}

### get NL attribute name from SQL attribute name
getNLattrNames <- function(SQLattrNames,allNLAttrNames){
  
  # get all attributes ($xxx) in strPattern, add "_" as an attribute "xx zzz" is stored as HCThead$xx_zzz
  allSQLattrNames = getSQLattrNames(allNLAttrNames)
  NLAttrNames = NULL
  for (i in 1:length(SQLattrNames)){
    NLAttrNames[i] = allNLAttrNames[allSQLattrNames %in% SQLattrNames[i]]
  }
  return(NLAttrNames)
}


# "of_$Gender_students" -> "of male students"
# "of_$Type_of_pollution_pollution" --> "of Inorganic Fertilizers pollution"
# HCTheadNames: list of all headers
# HCThead: a single attribute at a time, otherwise call geneRowColUnionFromNLPattern
# $A_bbb_$D_ccc returns a bbb ccc if colnames(HCThead) == "A", with "a" the value HCThead$A
# $A_bbb_$D_ccc returns bbb d ccc if colnames(HCThead) == "D", with "d" the value of HCThead$D
# $A_bbb_$D_ccc returns "" if colnames(HCThead) != "A" != "D",
getVarValueNLQ <- function(HCThead,strPattern,allNLattrNames,attrDelimiter=NULL){
  
  # get all attribute names existing in strPatterns and expected from the type of table
  validAttrNames = getValidAttrNames(strPattern,allNLattrNames)
  
  attrNames = colnames(HCThead)
  
  cnt = 0
  
  resCUR = strPattern
  for (ftname in validAttrNames){
    # "of_$Gender_students" "of_$Type_of_pollution_pollution" "of_$Polluted_element" "in_$City"
    splitSTR = unlist(strsplit(resCUR,split=paste0("\\$",ftname))) # if (varName == ..._$ftname_...) : ["of_$" "_students"] ["of_$" "_pollution"] ["of_$"] ["in_$"] else varName 
    
    # assume attrName == "Nationality"
    if (ftname %in% attrNames){
      # ftname = "Nationality"
      # ["of_" "_$Gender_students"] ["of_" "_pollution"]
      resSTART = splitSTR[1] # "of_" "of_"
      resCORE = HCThead[[ftname]] # "Qatari" "Inorganic Fertilizers"
      if (length(splitSTR)>1){ resEND = splitSTR[2]  # "_$Gender_students" "_pollution"
      } else {resEND = ""} 
      if (!is.null(attrDelimiter)){
        if (length(attrDelimiter)==2){
          resCORE = paste0(attrDelimiter[1],resCORE,attrDelimiter[2]) # "of_[Qatari]_$Gender_students" "of_[Inorganic Fertilizers]_pollution"
        }
      }
      resCUR = paste0(resSTART, resCORE, resEND) # "of_Qatari_$Gender_students" "of_Inorganic Fertilizers_pollution"
    } else {
      # ftname = "Gender"
      # ["of_$Nationality_" "_students"] 
      resCUR = paste0(splitSTR,collapse = "") # "of_$Nationality__students" "of_Qatari__students" "of_Inorganic Fertilizers_pollution"
      cnt = cnt +1
    }
  }

  if (cnt == length(validAttrNames)){
    # none of the attributes in strPattern matches with the candidate attribute in HCThead
    res = ""
  } else {
    res = gsub("_"," ",resCUR) # "of Qatari students"
    if (!is.null(attrDelimiter)){
      if (length(attrDelimiter)==2){
        res = resCUR # "of_##Nationality@@_students"
      }
    } 
  } 
  return(res)
}


# used to generate "for which" statements
# attrName = "Gender"
# attrPattern = "of_$Gender_students" -> "of gender", "in_$Language" -> "in language" 
# return the NL version of the pattern attrPattern if it contains a match with the attribute attrName
# "of_$Type_of_pollution_pollution" c("Type_of_pollution", "Gender", "Year") -> "of Type of pollution"
# "of__$Subtype__$Food_type" & attrName = "Subtype"    => "of Subtype"
# "of__$Subtype__$Food_type" & attrName = "Food_type"  => "of Food type"
# "of_$Subtype__$Food_type"  & attrName = "Subtype"    => "of Subtype"
# "of_$Subtype__$Food_type"  & attrName = "Food_type"  => "Food type"
# "of_$Subtype__with_$Food_type"    & attrName = "Food_type"   => "with Food type"
# "of__$Subtype_of_food__$Food_type" & attrName = "Subtype"   => "of Subtype"
# "of__$Subtype_of_food__$Food_type" & attrName = "Food_type"   => "of Food type"
# "in__$Quarter==of==$Year"          & attrName = "Year"    => "in Year"
# "in__$Quarter==of==$Year"          & attrName = "Quarter" => "in Quarter"
# return NULL otherwise
# attrPattern shall not contain " " spaces, it is one of a list of patterns after a split on " "
getVarAttrNLQ <- function(attrPattern,attrName,allNLattrNames,attrDelimiter=NULL){
  
  # attrPattern shall not contain " " spaces
  # check if the attribute is in the pattern
  splitATTR = unlist(strsplit(attrPattern,split=paste0("\\$",attrName)))
  if (splitATTR[1]==attrPattern) {
    # no split, no attribute in the pattern
    return(NULL)
  } else {
    # the attribute is in the pattern
    # get the NL version of the (SQL) attribute name
    attrNameNL = getNLattrNames(attrName,allNLattrNames)
    
    split2US = unlist(strsplit(attrPattern,split="__"))
    split2US2EQ = unlist(strsplit(split2US,split="=="))
    
    if (split2US2EQ[1] == attrPattern){
      # no split at all, there is a single attribute
      # a_$attr_z
      # we keep only the prefix part => "a attrNL"
      prefSTR = gsub("_"," ",splitATTR[1])
      return(stringr::str_squish(paste0(prefSTR," ",attrNameNL)))
      
    } else {
      # there is a split, we have several attributes in the pattern
      if (grepl("$",split2US2EQ[1],fixed = TRUE) == FALSE){
        # first segment does not contain an attribute
        # of__$attr...
        # it is a prefix for any of the attributes to come, among which our attribute
        prefSTR = gsub("_"," ",split2US2EQ[1])
        return(stringr::str_squish(paste0(prefSTR," ",attrNameNL)))
        
      } else {
        # first segment contains an attribute
        # of_$attr...
        # no prefix, we go over all attribute
        for (strCur in split2US2EQ){
          splitCUR = unlist(strsplit(strCur,split=paste0("\\$",attrName)))
          if (splitCUR[1] != strCur){
            # the attribute is there
            prefSTR = gsub("_"," ",splitCUR[1])
            return(stringr::str_squish(paste0(prefSTR," ",attrNameNL)))
          }
        }
      }
    }
  }
}
  

   

#  geneForWhichPremAttr(c("Year","Language"),"graduations of_$Gender_students with_a_$Degree in_$Language in_$Year ?") 
# -> "in Year and in Language" for which...
# "of_$Gender_students" & attr = "Gender" => "of Gender"
# "of__$Subtype__$Food_type" & attr = "Subtype"    => "of Subtype"
# "of__$Subtype__$Food_type" & attr = "Food_type"  => "of Food type"
# "of_$Subtype__$Food_type"  & attr = "Subtype"    => "of Subtype"
# "of_$Subtype__$Food_type"  & attr = "Food_type"  => "Food type"
# "of_$Subtype__with_$Food_type"    & attr = "Food_type"   => "with Food type"
# "of__$Subtype_of_food__$Food_type" & attr = "Subtype"   => "of Subtype"
# "of__$Subtype_of_food__$Food_type" & attr = "Food_type"   => "of Food type"
# "in__$Quarter==of==$Year"          & attr = "Year"    => "in Year"
# "in__$Quarter==of==$Year"          & attr = "Quarter" => "in Quarter"
geneForWhichPremAttr <- function(attrList, nlPattern,allNLattrNames) {
  print(attrList)
  print(nlPattern)
  splitPattern = unlist(strsplit(stringr::str_squish(nlPattern),split=" "))
  
  nlqPremAttr = NULL
  for (i in 1:length(attrList)){
    for (j in 1:length(splitPattern)){ 
      # if an attribute is used several times (It should not happen), the last occurrence is considered 
      NLattr = getVarAttrNLQ(splitPattern[j],attrList[i],allNLattrNames)
      if (!is.null(NLattr)){
         nlqPremAttr = c(nlqPremAttr,NLattr)
      }
    }
  }
  nlqPremSentence = getOxfordStyleGroup(nlqPremAttr," and ")
  
  return(nlqPremSentence)
}


##### NL version of EXPRESSIONS  $EXPR ######
getExprNLQ <- function(exprListSQL,segStr){
  #exprListSQL = SQL expressions    #  "sum" "min"
  #segStr = nlSegmentsNew[i] # "is/are_the_$EXPR_number/numbers"
  
  segList = unlist(strsplit(segStr,split="_")) # "is/are" "the" "$EXPR" "number/numbers"
  
  newStr = NULL
  for (i in 1:length(segList)){
    curVarName = segList[i]
    if (grepl("/", curVarName , fixed=TRUE)){ # "is/are" "number/numbers"
      curVarName1 = unlist(strsplit(curVarName,split="/"))
      if (length(exprListSQL)>1) { # "are" "numbers"
        newStr[i] = curVarName1[2]
      } else { # "is" "number"
        newStr[i] = curVarName1[1]
      }
    } else if (curVarName == "$EXPR") { # "$EXPR"
      exprList = unlist(lapply(exprListSQL,function(l){exprListName[[l]]}))
      newStr[i] = getOxfordStyleGroup(exprList," and ") 
    } else {
      newStr[i] = curVarName
    }
  }
  return(paste0(newStr,collapse = " "))
}

##### NL version of GROUPBY $GROUPBY ######
getGroupByNLQ <- function(groupByAttrNames,segStr,allNLattrNames){
  #groupByAttrNames =  colnames(HCTcolHeaders)    #  "Gender" "Year"
  #segStr = nlSegmentsNew[i] # "$GROUPBY"
  
  segList = unlist(strsplit(segStr,split="_")) # "is/are" "the" "$EXPR" "number/numbers"
  
  newStr = NULL
  for (i in 1:length(segList)){
    curVarName = segList[i]
    if (grepl("/", curVarName , fixed=TRUE)){ # "is/are" "number/numbers"
      curVarName1 = unlist(strsplit(curVarName,split="/"))
      if (length(groupByAttrNames)>1) { # "are" "numbers"
        newStr[i] = curVarName1[2]
      } else { # "is" "number"
        newStr[i] = curVarName1[1]
      }
    } else if (curVarName == "$GROUPBY") { # "$EXPR"
      groupByNLattrNames = getNLattrNames(groupByAttrNames,allNLattrNames)
      newStr[i] = gsub("_", " ",paste0(" for each ",getOxfordStyleGroup(groupByNLattrNames," and "),", ")) 
    } else {
      newStr[i] = curVarName
    }
  }
  return(paste0(newStr,collapse = " "))
}


##### NLQ REPORT ATTRIBUTES $REPORTATTR #####
geneReportAttr <- function(reportAttrNames,segStr){
  segList = unlist(strsplit(segStr,split="_")) # "is/are" "the" "$EXPR" "number/numbers"
  
  newStr = NULL
  for (i in 1:length(segList)){
    curVarName = segList[i]
    if (grepl("/", curVarName , fixed=TRUE)){ # "is/are" "number/numbers"
      curVarName1 = unlist(strsplit(curVarName,split="/"))
      if (length(reportAttrNames)>1) { # "are" "numbers"
        newStr[i] = curVarName1[2]
      } else { # "is" "number"
        newStr[i] = curVarName1[1]
      }
    } else if (curVarName == "$REPORTATTR") { # "$EXPR"
      newStr[i] = gsub("_", " ",paste0(getOxfordStyleGroup(reportAttrNames," and "),".")) 
    } else {
      newStr[i] = curVarName
    }
  }
  return(paste0(newStr,collapse = " "))
}

### generate NLQ for orderby clause, orderByK = c(K, DIR) 
# K=0 -> ORDER BY, K>0 TOP/BOTTOM K, DIR = DESC/ASC
getOrderByNLQ <- function(orderByK,segStr){
  
  #orderByK$K = 3
  #orderByK$orderBy = "DESC"
  #segStr = "$ORDERBYDESC_decreasing/increasing_values" # if firstK <= 0
  #segStr = "$ORDERBYDESC_descending/ascending_values"  # if firstK <= 0
  #segStr = "top/bottom_$TOPK"        # if firstK > 0
  #segStr = "largest/smallest_$TOPK"  # if firstK > 0
  
  segList = unlist(strsplit(segStr,split="_")) # "top/bottom" "$K" "descending/ascending"
  
  # orderByK = c(K,DIR)
  firstK  = orderByK$K # K=0 -> ORDER BY, K>0 TOP/BOTTOM K,
  rankDir = orderByK$orderBy # DIR = DESC/ASC   # X/Y, X always means descending or top, Y ascending or bottom
  
  
  newStr = NULL
  for (i in 1:length(segList)){
    curVarName = segList[i]
    if (grepl("/", curVarName , fixed=TRUE)){ # "top/bottom" "descending/ascending"
      curVarName1 = unlist(strsplit(curVarName,split="/"))
      if (rankDir == "DESC") { # TOP, DESCENDING
        newStr[i] = curVarName1[1]
      } else { # BOTTOM, ASCENDING
        newStr[i] = curVarName1[2]
      }
    } else if (curVarName == "$ORDERBYDESC" || curVarName == "$TOPK") { 
      if (firstK <= 0) {
        newStr[i] = "ordered by"
      } else {
        newStr[i] = firstK 
      }
    } else {
      newStr[i] = curVarName
    }
  }
  
  return(paste0(newStr,collapse = " "))
  
}


# "What is/are_the_$OPATTR_for_which the number of graduations 
# of_$Gender_students with_a_$Degree in_$Language in_$Year is_$OPANDVAL ?"
# forWhich$OPATTR <- attrList
# forWhich$OPTYPE <- operationOP
# forWhich$OPVAL <- operationVAL
getForWhichNLQ <- function(forWhich,segStr){
  
  segList = unlist(strsplit(segStr,split="_")) # "is/are_the_$OPATTR_for_which" ... "is_$OPANDVAL"
  
  attrList = forWhich$OPATTR
  operationOP = str_squish(forWhich$OPTYPE)
  
  # https://www.tutlane.com/tutorial/sql-server/sql-comparison-operators
  if (operationOP == ">") operationOP = "greater than"
  if (operationOP == ">=") operationOP = "greater than or equal to"
  if (operationOP == "<") operationOP = "lower than"
  if (operationOP == "<=") operationOP = "lower than or equal to"
  if (operationOP == "=") operationOP = "equal to"
  if (operationOP == "!=") operationOP = "different from"
  if (operationOP == "<>") operationOP = "different from"
  #  if (operationOP == "!<") operationOP = "not lower than"
  #  if (operationOP == "!>") operationOP = "not greater than"
  
  operationVAL = forWhich$OPVAL
  
  newStr = NULL
  for (i in 1:length(segList)){
    curVarName = segList[i]
    if (grepl("/", curVarName , fixed=TRUE)){ # "is/are"
      curVarName1 = unlist(strsplit(curVarName,split="/"))
      if (length(attrList) == 1) { # is
        newStr[i] = curVarName1[1]
      } else { # are
        newStr[i] = curVarName1[2]
      }
    } else if (curVarName == "$OPATTR") { 
      newStr[i] = getOxfordStyleGroup(attrList," and ") 
    } else if (curVarName == "$OPANDVAL") { 
      newStr[i] = paste(operationOP,operationVAL) 
    } else {
      newStr[i] = curVarName
    }
  }
  
  return(paste0(newStr,collapse = " "))
  
}

# Get NLQ express for an attribute names
# getNLQfromAttr(c("graduations","of_$Gender_students", "with_a_$Degree", "in_$Year"),"Year" )
# getNLQfromAttr <- function(nlSegList,attrName){
#   for (i in 1:length(nlSegList)){
#     varNm = getAttrNamesFromPattern(nlSegList[i],attrName)
#     if (!is.null(varNm)){
#       if (attrName == getAttrNamesFromPattern(nlSegList[i],attrName)) return(nlSegList[i])
#     }
#   }
# }

# improve paste0(...,collapse=" or ") or ...,collapse=" and ")
# collapseNoRep(c("a")," or ") --> "a"
# collapseNoRep(c("a","b")," or ") --> "a or b"
# collapseNoRep(c("a","b","c")," or ") --> "a, b, or c"
# collapseNoRep(c("a","b","c","d")," or ") --> "a, b, c, or d"
# collapseNoRep <- function(strListToGroup, sep = " and "){
#   
#   if (length(strListToGroup)<3) {
#     return(paste0(strListToGroup,collapse=sep))
#   } else {
#     numVal = length(strListToGroup)
#     commaSep = paste0(",",sep)
#     return(paste0(c(paste0(strListToGroup[1:(numVal-1)],collapse= ", "),strListToGroup[numVal]),collapse=commaSep))
#   }
# }

# Gene NLQ from ROW/COL OR clause, factorizing repeated tokens
# Preserve order of tokens in nlRowColPattern
# number of female or male students with a PhD or BSc level in Arabic in 2015/2016
# NO : too difficult to factorize preserving order does not work
# DONE: If a single token varies: express the OR within the token, keep the rest constant.
# DONE: If two token varies and lead to a complet 00,01,10,11 combinaiton, vary them in place,
# DONE: in any other case, replicate each row of the OR clause in full
# HCTelt: data frame of the rows or columns to express as a OR NL clause
# nlRowColPattern: list of NL patterns for each attribute name, 
# the order may be different between nlRowColPattern and the columns of HCTelt
# HCTelt
# Year Language
# 2017/2018   Arabic
# 2015/2016  English
# 2016/2017  English
# nlRowColPattern 
# "in_$Language" "in_$Year" 
# If several attribute in one pattern, use double-underscore "__" to separate groups 
# "of_$Nationality__$Gender_students__in_$Year"
# Nationality Gender Year
# 1      Qatari Female 2020
# 2  Non-Qatari Female 2020
# 3     Turkish Female 2020
# => "of Qatari, Non-Qatari, or Turkish Female students in 2020"
# Nationality Gender Year
# 1      Qatari Female 2020
# 2  Non-Qatari Female 2020
# 3     Turkish Female 2021
# => "of Qatari Female students in 2020, of Non-Qatari Female students in 2020, or of Turkish Female students in 2021"
# Nationality Gender Year
# 1      Qatari Female 2020
# 2  Non-Qatari Female 2020
# 3      Qatari Female 2021
# 4  Non-Qatari Female 2021
# => "of Qatari or Non-Qatari Female students in 2020 or 2021"
geneRowColUnionFromNLPattern <- function(HCTelt,nlRowColPattern,allNLattrNames,attrDelimiter=NULL){
  if (!is.null(HCTelt)){
    
    # get the attr names in the pattern valid in the current table
    validAttrNames = getValidAttrNames(paste0(nlRowColPattern,collapse = "__"),allNLattrNames)
    
    # keep only the columns of the HCTelt having these attribute names
    attrNames = colnames(HCTelt)
    
    attrKeep = NULL
    for (ftname in validAttrNames){
      if (ftname %in% attrNames) attrKeep = c(attrKeep, ftname)
    }
    HCTelt = unique(HCTelt[,attrKeep,drop=FALSE])
    
    # Split further grouped attributes
    nlRowColPatternClean = NULL
    nlRowColPattern = unlist(strsplit(nlRowColPattern,split = "__"))
    
    # nlRowColPattern has been split by "__"
    # z_a__b_$C_d__e_f__g_$H_i__j_k => "z_a" "b_$C_d" "e_f" "g_$H_i" "j_k"   
    # z_a__b_$C_d==e_f==g_$H_i__j_k => "z_a" "b_$C_d==e_f==g_$H_i" "j_k" 
    #nlRowColPattern="z_a__b_$C_d==e_f==g_$H_i__j_k"
    nlPatClean = NULL
    nlPatLabel = NULL
    split2US = unlist(strsplit(nlRowColPattern,split = "__"))
    for (ii in 1:length(split2US)){
      split2EQ = unlist(strsplit(split2US[ii],split = "=="))
      nlPatClean = c(nlPatClean,split2EQ)
      if (length(split2EQ)==1) { # no ==
        nlPatLabel = c(nlPatLabel,FALSE)
      } else {
        nlPatLabel = c(nlPatLabel,!grepl("$",split2EQ,fixed=TRUE))
      }
    }
    nlRowColPattern = nlPatClean
    nlRowColLabel = nlPatLabel # TRUE if corresponding pattern B is $A==B==$C, B is deleted if A or C is absent
      
    # get columns where all values are identical (to be factorized)
    dup = rep(FALSE,ncol(HCTelt))
    if (nrow(HCTelt)>1){
      for (ii in 1:ncol(HCTelt)){
        if (length(unique(HCTelt[,ii])) == 1) dup[ii]=TRUE # the attribute values never change
      }
    }
    # if a single attribute varies: 
    indColFixed = which(dup)
    indColVaried = which(!dup)
    numColFixed = length(indColFixed)
    numColVaried = length(indColVaried)
    geneIndividualOR = FALSE # default: generate all OR clauses without factorizing
    if (numColVaried == 1) {
      # a single attribute varies and all others are constant 
      # if only Year varies we can generate "in English in 2015/2016 or 2016/2017" 
      geneIndividualOR = TRUE
    }
    if (numColVaried > 1) {
      colAllUnique = NULL
      colAllObserved = HCTelt[,indColVaried]
      for (icol in 1:ncol(colAllObserved)){
        colAllUnique[[icol]] = unique(colAllObserved[,icol])
      }
      
      allUniqueCombi = expand.grid(colAllUnique,stringsAsFactors = FALSE) # all combination of elements for both columns
      rowUniqueCombi = NULL
      for (irow in 1:nrow(allUniqueCombi)){
        rowUniqueCombi[irow]= paste0(unlist(allUniqueCombi[irow,]),collapse = "_")
      }
      
      allUniqueCombiSignature = sort(rowUniqueCombi) # all combinations sorted
      
      rowObservedCombi = NULL
      for (irow in 1:nrow(colAllObserved)){
        rowObservedCombi[irow]= paste0(unlist(colAllObserved[irow,]),collapse = "_")
      }
      allObservedCombiSignature = sort(rowObservedCombi) # observed combinations sorted
    
    
      if (length(allUniqueCombiSignature) == length(allObservedCombiSignature)){
        if (all(allUniqueCombiSignature == allObservedCombiSignature)) {
          # the observed combinations are exactly the same as all possible combinations
          # we can generate OR clause for each varying attribute individually
          # we can generate "in English or Spanish in 2015/2016 or 2016/2017"
          geneIndividualOR = TRUE
        }
      }
    }  
    
    
    
    if (FALSE){ #geneIndividualOR == TRUE){
      # generate OR clause for each varying attribute individually
      curSeg = NULL
      curLab = NULL
      
      for (j in 1:length(nlRowColPattern)){ # generate attributes in same order as in pattern
        curPattern = nlRowColPattern[j] 
        curLabel = nlRowColLabel[j]
        curSeg[j] = ""
        curLab[j] = curLabel
        curAttr = getAttrNamesFromPattern(curPattern,colnames(HCTelt)) 
        if (!is.null(curAttr)){
          if (grepl("$", curPattern , fixed=TRUE)){ 
            if (any(curAttr %in% colnames(HCTelt))){
              newFirstSeg = getVarValueNLQ(HCTelt[1,,drop=FALSE],curPattern,allNLattrNames,attrDelimiter)
              allVal = unique(as.character(HCTelt[[curAttr]])) # all alternative values of the attribute
              newCombinedSeg = gsub(HCTelt[[curAttr]][1],getOxfordStyleGroup(allVal," or "),newFirstSeg)
              curSeg[j] = newCombinedSeg
            } 
          }
        } else {
          if (!grepl("$", curPattern , fixed=TRUE)){
            curSeg[j] = gsub("_"," ",curPattern) 
          }
        }
      }
      
      # clean up: remove B in $A==B==$C if A or C is empty 
      nlKeep = !curLab
      ind2EQ = which(curLab)
      for (ii in ind2EQ){
        if (nchar(curSeg[ii-1])>0 && nchar(curSeg[ii+1])>0) nlKeep[ind2EQ]=TRUE
      }
      
      curRowRep = paste0(curSeg[nlKeep],collapse=" ")
      startSegMerged = ""
    } else {
      # generate all OR clause row by row of HCTelt
      curRowRep = NULL
      firstAttrGlobal = FALSE
      kk = 0
      startSeg = NULL
      for (i in 1:nrow(HCTelt)){
        curSeg = NULL
        curLab = NULL
        firstAttrLocal = FALSE
        jj = 0
        for (j in 1:length(nlRowColPattern)){ # generate attributes in same order as in pattern
          curPattern = nlRowColPattern[j]
          curLabel = nlRowColLabel[j]
          curSeg[j] = ""
          curLab[j] = curLabel
          if (grepl("$", curPattern , fixed=TRUE)){ 
            NLQvar = getAttrNamesFromPattern(curPattern,colnames(HCTelt))
            if (!is.null(NLQvar)){
              if (any(NLQvar %in% colnames(HCTelt))){  
                curSeg[j] = getVarValueNLQ(HCTelt[i,,drop=FALSE],curPattern,allNLattrNames,attrDelimiter)
                firstAttrGlobal = TRUE
                firstAttrLocal = TRUE
              }
            } 
          } else {
            if (firstAttrGlobal == FALSE){ # do not repeat the first segments before an attribute when generating or clauses.
              kk = kk + 1
              startSeg[kk] = gsub("_"," ",curPattern) # "of_the_$Attr1" => "of_the" => "of the"  
            } else if (firstAttrLocal == TRUE){
              curSeg[j] = gsub("_"," ",curPattern) 
            }
          }
        }
        
        # clean up: remove B in $A==B==$C if A or C is empty 
        nlKeep = !curLab
        ind2EQ = which(curLab)
        for (ii in ind2EQ){
          if (nchar(curSeg[ii-1])>0 && nchar(curSeg[ii+1])>0) nlKeep[ind2EQ]=TRUE
        }
        
        curRowRep[i] = paste0(curSeg[nlKeep],collapse=" ")
      }
      startSegMerged = paste0(startSeg,collapse = " ")
    }
    NLQstr = paste0(startSegMerged," ",getOxfordStyleGroup(curRowRep," or "))
    
  } else {
    NLQstr = ""
  }
  
  return(NLQstr)
}

### split a sentence to isolate attribute names
### sentence = "number of_##Non-Qatari@@ ##Male@@_students in_##Government@@_schools in_##2021/2022@@"
### attrDelimiter = c("##","@@")
### splitAttr(sentence,attrDelimiter) => c("of ", "##Non-Qatari@@", " ", "##Male@@", " students in ", "##Government@@", " schools in ", "##2021/2022@@")  
splitAttr1<-function(sentence,attrDelimiter){
  
  s1 = unlist(strsplit(sentence,split = attrDelimiter[1])) # "##"
  s1a = paste0(attrDelimiter[1],s1)
  s1b = unlist(lapply(s1a,function(x){grepl(attrDelimiter[1],x)&&grepl(attrDelimiter[2],x)}))
  s1c = s1
  for (iii in 1:length(s1)){ if (s1b[iii]) {s1c[iii] = s1a[iii]}}

  s2 = unlist(strsplit(s1c,split = attrDelimiter[2])) # "@@"
  s2a = paste0(s2,attrDelimiter[2])
  s2b = unlist(lapply(s2a,function(x){grepl(attrDelimiter[1],x)&&grepl(attrDelimiter[2],x)}))
  s2c = s2
  for (iii in 1:length(s2)){ if (s2b[iii]) {s2c[iii] = s2a[iii]}}
  
  return(s2c)
}

### split a sentence to isolate attribute names but preserve prefix and suffix
### sentence = "number of_##Non-Qatari@@ ##Male@@_students in_##Government@@_schools in_##2021/2022@@"
### attrDelimiter = c("##","@@")
### splitAttr(sentence,attrDelimiter) => c("of_##Non-Qatari@@", " ", "##Male@@_students", "in_##Government@@_schools", "in_##2021/2022@@")  
splitAttr2<-function(sentence,attrDelimiter){
  s0 = unlist(strsplit(stringr::str_squish(sentence),split = " "))
  # "of_##Inferior" "Irrigation@@_pollution" "of_##Soil@@" "in_##Flint" "or" "Port" "Huron@@" ",_##Michigan@@" "in_##2019" "or" "2020@@"
  cumulSeg = NULL
  resSeg = NULL
  flagOn = FALSE
  for (i in 1:length(s0)){
    curSeg = s0[i]
    delimStart = grepl(attrDelimiter[1],curSeg)
    delimEnd = grepl(attrDelimiter[2],curSeg)
    if (delimStart && delimEnd){
      resSeg = c(resSeg,curSeg)
    } else if (delimStart){
      flagOn = TRUE
      cumulSeg = c(cumulSeg,curSeg)
    } else if (delimEnd){
      flagOn = FALSE
      cumulSeg = c(cumulSeg,curSeg)
      resSeg = c(resSeg,paste0(cumulSeg,collapse = " "))
      cumulSeg = NULL
    } else {
      if (flagOn) {
        cumulSeg = c(cumulSeg,curSeg)
      } else {
        resSeg = c(resSeg,curSeg)
      }
    }
  }
  # "of_##Inferior Irrigation@@_pollution" "of_##Soil@@" "in_##Flint or Port Huron@@" ",_##Michigan@@" "in_##2019 or 2020@@"
  return(resSeg)
}

# trimAttr(c("##Female@@","##Male@@"),c("##","@@"))  => "Female" "Male"
trimAttr <- function(sentences,attrDelimiter){
  sres = NULL
  for (sent in sentences){
    s1 = unlist(strsplit(sent,split = attrDelimiter[1]))
    s2 = unlist(strsplit(s1,split = attrDelimiter[2]))
    sres = c(sres,s2)
  }
  return(sres)  
}

# mergeAttr(c("Female","Male"),"<OR>")  => "Female<OR>Male" 
mergeAttr <- function(sentences,mergeStr){
  return(paste0(sentences,collapse=mergeStr))
} 

# addAttrDelimiterAttr("Female<OR>Male",c("##","@@"))  => "##Female<OR>Male@@"
addAttrDelimiterAttr <- function(sentence,attrDelimiter){
  return(paste0(attrDelimiter[1],sentence,attrDelimiter[2]))
}

# regroupAttr(c("##Female@@","##Male@@"),c("##","@@"),"<OR>")  => "##Female<OR>Male@@"
regroupAttr <-function(sentences,attrDelimiter,mergeStr){
  #return(addAttrDelimiterAttr(mergeAttr(trimAttr(sentences,attrDelimiter),mergeStr),attrDelimiter))
  return(addAttrDelimiterAttr(getOxfordStyleGroup(trimAttr(sentences,attrDelimiter),mergeStr),attrDelimiter))
  
}

# regroupAttr(c("pref_##Female@@_suff","pref_##Male@@_suff"),c("##","@@"),"<OR>")  => "pref_##Female_suff<OR>pref_Male@@_suff"
regroupAttr2 <-function(sentences,attrDelimiter,mergeStr){
  #return(addAttrDelimiterAttr(mergeAttr(trimAttr(sentences,attrDelimiter),mergeStr),attrDelimiter))
  mergeSent = paste0("ZZZZZ",mergeAttr(sentences,mergeStr),"ZZZZZ") # add ZZZZZ at start and end to avoid issues when delimiters are there too
  
  mergeSent1 = unlist(strsplit(mergeSent,split=attrDelimiter[1]))
  mergeSent2 = unlist(strsplit(mergeSent,split=attrDelimiter[2]))
  mergeSent12 = unlist(strsplit(mergeSent1,split=attrDelimiter[2]))
  
  midSeg12 = paste0(mergeSent12[2:(length(mergeSent12)-1)],collapse = "")
  resSeg = gsub("ZZZZZ","",paste0(mergeSent12[1],attrDelimiter[1],midSeg12,attrDelimiter[2],mergeSent12[length(mergeSent12)]))
  
  return(resSeg)
}


## FOR TEST
attrDelimiter = c("##","@@")
#allSent=c("of_##Inferior Irrigation@@_pollution of_##Soil@@ which are located in_##Flint or Port Huron@@ in the ,_##Michigan@@ during time in_##2019 or 2020@@",       
#          "of_##Inferior Irrigation@@_pollution of_##Soil@@ which are located in_##Hastings, Minneapolis, or Rochester@@ in the ,_##Minnesota@@ during time in_##2019 or 2020@@")
# A #B@ C #D@ E #F@ G #H@ I
# A #B@ C #X@ E #Y@ G #H@ I
# _  0  _  1  _  1  _  0  _  = 0...0110...0  (series of contiguous 1 counting only $, then remove internal @@...## to group them)
# 0110 
# 010 = 1
# 1 group of 1, regroup it
# A #B@ C #D@ E #F@ G #H@ I => A #B@ C pref_#D E F@_suf G #H@ I
# A #B@ C #X@ E #Y@ G #H@ I => A #B@ C pref_#X E Y@_suf G #H@ I
# merge including pref and suf: A #B@ C prf_#D E F_suf or pref_X E Y@_suf G #H@ I

# A #B@ C #D@ E #F@ G #H@ I #J@
# A #B@ C #X@ E #Y@ G #H@ I #Z@
# _  0  _  1  _  1  _  0  _  1 = 0...0110...01... No more on component of 1 so no grouping 
# 01101
# 0101 = 2
# do not group

# allSent = c("of_##Surface water@@_pollution of_##Water@@ in_##Hastings@@ ,_##Minnesota@@",   
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Minneapolis@@ ,_##Minnesota@@",
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Rochester@@ ,_##Minnesota@@",  
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Boulder City@@ ,_##Nevada@@",  
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Reno@@ ,_##Nevada@@",          
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Portland@@ ,_##Oregon@@",      
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Redmond@@ ,_##Oregon@@",       
#             +             "of_##Surface water@@_pollution of_##Water@@ in_##Salem@@ ,_##Oregon@@")

regroupSentencesLastPass <- function(allSent,attrDelimiter){
  
  # print("---------------- regroupSentencesLastPass ------------------")
  
  # attrDelimiter = c("##","@@")
  # allSent= c("of_##Inferior Irrigation or Inorganic Fertilizers@@_pollution of_##Soil@@ in_##Baltimore@@ ,_##Maryland@@", 
  #            "of_##Light or Noise@@_pollution of_##Air@@ in_##Baltimore@@ ,_##Maryland@@",                                
  #            "of_##Inferior Irrigation or Inorganic Fertilizers@@_pollution of_##Soil@@ in_##Lenox@@ ,_##Massachusetts@@",
  #            "of_##Light or Noise@@_pollution of_##Air@@ in_##Lenox@@ ,_##Massachusetts@@")                               
  #  
  
  # allSent= c("of_##Solid Waste@@_pollution of_##Soil@@ in_##Columbus@@ ,_##Georgia@@ in_##2018 or 2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Tampa@@ ,_##Florida@@ in_##2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Albany@@ ,_##Georgia@@ in_##2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Champaign@@ ,_##Illinois@@ in_##2019@@")
  # allSent= c("of_##Solid Waste@@_pollution of_##Soil@@ in_##Tampa@@ ,_##Florida@@ in_##2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Albany@@ ,_##Georgia@@ in_##2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Columbus@@ ,_##Georgia@@ in_##2018 or 2019@@",
  #            "of_##Solid Waste@@_pollution of_##Soil@@ in_##Champaign@@ ,_##Illinois@@ in_##2019@@")
  
  # allSent= c("of_##Oxygen-depletion@@_pollution of_##Water@@ in_##Baltimore@@ ,_##Maryland@@",                          
  #            "of_##Industrial, Solid Waste, or Urban Activities@@_pollution of_##Soil@@ in_##Baltimore@@ ,_##Maryland@@",
  #            "of_##Light@@_pollution of_##Air@@ in_##Baltimore@@ ,_##Maryland@@")
  # 
  
  
  allSentTmp = unique(allSent)
  numSent = length(allSentTmp)
  
  if (numSent>1){
    
    vecrep = 1+(0:(2*numSent-1))%%(numSent) # 1,2,3,4 => 1,2,3,4,1,2,3,4
    
    res = list()
    for (zzz in 1:numSent){
      allSent = allSentTmp[vecrep[zzz:(zzz+numSent-1)]]
      
      allSentMerge = NULL
      #allSentMergeIndex = NULL
      
      # print("@@@@@@@@@@@@@@@@@@ NEW PASS @@@@@@@@@@@@@@@@@@@@")
      
      stop2 = FALSE
      k2 = 0
      while (stop2 == FALSE && length(allSent)>1){
        # print(paste0("=== ALL SENT ",k2," === ",allSent))
        k2 = k2 + 1 
        allSentMerge = NULL # reinit
        # remove duplicates
        refSent = allSent[1]
        candSent = allSent[2:length(allSent)] # list of candidate sentences to merge
        stop1 = FALSE
        while(stop1 == FALSE){ # still some candidate to merge
          
          cntDiff = NULL
          locDiff = vector(mode = "list", length = length(candSent))
          sentDiff = vector(mode = "list", length = length(candSent))
          
          # print(paste0("REF_SENT === ",refSent))
          # print(paste0("CAND_SENT === ",candSent))
          
          # REF SENTENCE
          l11 =  splitAttr2(refSent,attrDelimiter)
          l11Attr = unlist(lapply(l11,function(l){if (grepl(attrDelimiter[1],l)) return(l)}))
          
          
          l22 = NULL
          l22Attr = NULL
          l12eqAttr = NULL
          doRegroup = NULL
          for (i2 in 1:length(candSent)) {
            # print(paste0("CAND_SENT i2 === ",candSent[i2]))
            
            l22[[i2]] = splitAttr2(candSent[i2],attrDelimiter)
            l22Attr[[i2]] = unlist(lapply(l22[[i2]],function(l){if (grepl(attrDelimiter[1],l)) return(l)}))
            
            
            #####################
            #####################
            
            # regroup consecutive varying attribute parts only if forming a single component between one or two fix attribute parts 
            doRegroup[i2] = FALSE
            
            ### check for all pairs of candidate, how many elementary part (attr) they have in common. 
            ###  s1 = A B C1 D1 
            ###  s2 = A B C2 D2
            ###  s3 = A B C3 D2  
            ###  s4 = A B C4 D1
            ### Start merging pairs which have the highest number of static pairs in common
            ### here start merging s2 and s3, et s1 et s4
            ###  s14 = A B (C1 or C4) D1 
            ###  s23 = A B (C2 or C3) D2
            ### then s14 and s23, because merging will destroy alignment
            ###  s1234 = A B ((C1 or C4) D1) or ((C2 or C3) D2))
            ### Sinon:
            ### si on merge s1 avec s2, s3 et s4 à cause de A et B, on obtient
            ###  s123 = A B ((C1 D1) or (C2 D2) or (C3 D2) or (C4 D1)) et on ne peut plus simplifier
            ###  voir 1455 template 3
            
            ### Autre BUG, on utilise des merging entre attributs dans cette deuxieme passe, 
            ### donc on ne peut plus comparer les parties mergee avec les non mergees, elles n ont plus le meme nombre de segment 
            ### faire un arbre, merger en premier ceux qui ont le plus de parties commune, puis remonter. si plusieurs exaequo, essayer les configurations
            ### et prendre cell qui merge le plus 
            
            ### SOLUTION 1: changer l ordre des candidat et faire plusieur tours. Choisir l ordre qui donne le moins de repetitions
            ### SOLUTION 2: quand la ref a deja ete mergee (au deuxieme passage outer while), il faut merger tous les candidats de la meme facon, 
            ### donc il faut memoriser les items merge par rapport à l'etape precedente
            
            # print("DEBUG DEBUG DEBUG START")
            # print(allSent)
            # print("====== l11")
            # print(l11Attr)
            # print("====== l22")
            # print(l22Attr[[i2]])
            # print("DEBUG DEBUG DEBUG END")
            
            if (length(l11Attr) == length(l22Attr[[i2]])){
              l12eqAttr[[i2]] = (l11Attr == l22Attr[[i2]])
              
              
              dif = diff(l12eqAttr[[i2]])
              if (sum(abs(dif))==1) {
                # TTFFF or FFTTTT
                doRegroup[i2] = TRUE
              } else if (sum(dif)==0 && sum(abs(dif))==2){
                indneg = which(dif==-1)
                indpos = which(dif==1) 
                if (length(indneg)>0 && length(indpos)){
                  if (indneg<indpos) {
                    # TTFFFFTTTT
                    doRegroup[i2] = TRUE
                  }
                }
              }
            }
          }  # END FOR LOOP
          
          mergedIndex = 0
          indSentToMerge = which(doRegroup)
          # print("indSentToMerge")
          # print(indSentToMerge)
          
          if (length(indSentToMerge)>0){
            
            # pick first one to determine common static parts
            indCur = indSentToMerge[1]
            
            difl12 = (l11!=l22[[indCur]])
            # print("l11")
            # print(l11)
            # print("l22[[indCur]]")
            # print(l22[[indCur]])
            # print("difl12  _1")
            # print(difl12)
            
            inddifl12 = which(difl12)
            indFirstDif = min(inddifl12)
            indLastDif = max(inddifl12)
            difl12[indFirstDif:indLastDif]= TRUE
            
            # print("difl12  _2")
            # print(difl12)
            # print("l11[difl12]")
            # print(l11[difl12])
            
            
            l11merged = mergeDelim(l11[difl12],attrDelimiter)
            
            mergedIndex = difl12
            # print("mergedIndex")
            # print(mergedIndex)
            # print("l11merged")
            # print(l11merged)
            
            l22merged = NULL
            curSignature = l12eqAttr[[1]] # the first one served as reference
            indSentToMergeActual = NULL
            for (irgp2 in 1:length(indSentToMerge)){
              if (all(curSignature == l12eqAttr[[irgp2]])){
                indSentCur =  indSentToMerge[irgp2]
                l22merged = c(l22merged,mergeDelim(l22[[indSentCur]][difl12],attrDelimiter))
                indSentToMergeActual = c(indSentToMergeActual,indSentCur)
              }
            } 
            # print("l22merged")
            # print(l22merged)
            
            sentToMerge = sort(c(l11merged,l22merged))
            
            l12merged = regroupAttr2(sentToMerge,attrDelimiter," or ")
            
            if (indFirstDif > 1) {
              l12start = paste0(l11[1:(indFirstDif-1)],collapse = " ")
            } else {
              l12start = ""
            }
            if (indLastDif < length(l11)) {
              l12end = paste0(l11[(indLastDif+1):length(l11)],collapse = " ")
            } else {
              l12end = ""
            }
            
            
            # print(paste0("$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ",sentToMerge))
            
            
            l12 = paste0(l12start," ",l12merged," ",l12end)
            
            # print(paste0("$$$$$$ MERGED $$$$$$$$$ ",l12))
            
            newSentMerge = l12 #paste0(l12,collapse="") # add the reference sentence merged with other 1-diff sentences 
            
            allSentMerge = c(allSentMerge,newSentMerge)
            #allSentMergeIndex[[length(allSentMergeIndex)+1]] = mergedIndex
            
            # remove merged sentence from candidate set
            candSentNew = NULL
            l22NEW = NULL
            jj=0
            for (j in 1:length(candSent)){
              if (!(j %in% indSentToMergeActual)) { 
                candSentNew = c(candSentNew,candSent[j])
                jj = jj+1
                l22NEW[[jj]] = l22[[j]]
              }
            }
            # merge attributes of candidates the same way to allow comparison in next round 
            # print("$$$$$$$$$$$ NEW CANDIDATES $$$$$$$$$$$")
            # print(candSentNew)
            # print(l22NEW)
            candSentNew2 = NULL
            for (jj in 1:length(l22NEW)){
              l22NEWcur = l22NEW[[jj]]
              # print(l22NEWcur)
              # print(mergedIndex)
              indMerge = which(mergedIndex)
              minInd = min(indMerge)
              maxInd = max(indMerge)
              lastInd = length(l22NEWcur)
              indStartNew=0
              indEndNew=0
              if (minInd>1) indStartNew = 1:(minInd-1)
              if (maxInd<lastInd) indEndNew = (maxInd+1):lastInd
              l22NEWstart = ""
              l22NEWend = ""
              if (indStartNew[1]>0) l22NEWstart = l22NEWcur[indStartNew]
              if (indEndNew[1]>0) l22NEWend = l22NEWcur[indEndNew]
              l22NEWmiddle = mergeDelim(l22NEWcur[indMerge],attrDelimiter)
              # print(l22NEWstart)
              # print(l22NEWmiddle)
              # print(l22NEWstart)
              candSentNew2[jj] = stringr::str_squish(paste0(c(l22NEWstart,l22NEWmiddle,l22NEWend),collapse = " "))
            }
            # print("$$$$$$$$$$$ NEW CANDIDATES BEFORE $$$$$$$$$$$")
            # print(candSentNew)
            # print("$$$$$$$$$$$ NEW CANDIDATES AFTER $$$$$$$$$$$")
            # print(candSentNew2)
            candSentNew = candSentNew2
            
            if (is.null(candSentNew)) { # no more sentence to test for merging 
              stop1 = TRUE
            } else if (length(candSentNew)==1){
              # add the last candidate to the set of result,
              if (nchar(candSentNew)>0){ 
                allSentMerge = c(allSentMerge,candSentNew)
                #allSentMergeIndex[[length(allSentMergeIndex)+1]] = mergedIndex
              }
              # and stop as nothing is let to be merged with it in the next round
              stop1 = TRUE
            } else {
              # more than one candidate sentence
              # pick the first one as the next reference
              refSent = candSentNew[1]
              candSent = candSentNew[2:length(candSentNew)]
            }
            # print(paste0("RESULT_SENT @@@ ",allSentMerge))
          } else {
            # Nothing to merge
            # Add the current reference to the results
            allSentMerge = c(allSentMerge,refSent)
            #allSentMergeIndex[[length(allSentMergeIndex)+1]] = mergedIndex
            
            candSentNew = NULL
            # pick the first one as the next reference
            refSent = candSent[1]
            if (length(candSent)==1) { 
              # it was the last candidate, add it to the set of results,
              allSentMerge = c(allSentMerge,refSent)
              #allSentMergeIndex[[length(allSentMergeIndex)+1]] = mergedIndex
              
              candSent=NULL
              # and stop as nothing is let to be merged with it in the next round
              stop1 = TRUE
            } else{ 
              for (j in 2:length(candSent)){
                candSentNew = c(candSentNew,candSent[j])
              }
              candSent = candSentNew
            }
            # print(paste0("CUR RESULT === ",allSentMerge))
          }
          
        } # END INNER WHILE
        
        if (length(allSentMerge) == length(allSent)) { 
          # no merging occured, stop
          # print("********** NO MERGING OCCURED *************")
          # print("********************************")
          # print(allSentMerge) 
          # print(allSentMergeIndex) 
          # print("********************************")
          
          stop2 = TRUE 
        } else {
          # some merging occured, loop again on the remaining attributes
          allSent = allSentMerge
          
          # print("********** MERGING OCCURED *************")
          # print("********************************")
          # print(allSent) 
          # print(allSentMergeIndex) 
          # print("********************************")
          
         
        }
        if (k2 > 5) stop2 = TRUE
        
        allSent = unique(allSent)
        # print("********** allSent before next pass START ************")
        # print(allSent)
        allSentNEXT = NULL
        for (ddd in 1:length(allSent)){
          curSent = stringr::str_squish(allSent[ddd])
          if (nchar(curSent)>0)
            allSentNEXT = c(allSentNEXT, curSent)
        }
        allSent = allSentNEXT
        # print("********** allSent before next pass END *************")
        
      } # END OUTER WHILE 
      
      # print("RES __1")
      # print(allSentMerge)
      # print("RES __2")
      # print(res)
      res[[length(res)+1]] = allSentMerge
      
    } # END FOR ZZZ
    
    # print("=========== RES ===========")
    # print(res)
    # print("=========== --- ===========")
    
    numSol=NULL
    for (rrr in 1:length(res)){
      numSol[rrr] = length(res[[rrr]])
    }
    # print(numSol)
    numMin = min(numSol)
    indMin = which(numSol==numMin)
    # print(indMin)
    
    score = NULL
    indScore = NULL
    for (rrr in 1:length(indMin)){
      numChar = 0
      for (sss in 1:numMin){
        numChar = numChar + nchar(res[[indMin[rrr]]][sss])
      }
      score[rrr]=numChar
      indScore[rrr]=indMin[rrr]
    }
    indMinScore = which.min(score)
    # print(score)
    # print(indScore)
    # print(indMinScore)
    
    resFinal = res[[indScore[indMinScore]]]
    # print("=========== RES FINAL===========")
    # print(resFinal)
    # print("=========== --- ===========")
    
  } else {
    allSentMerge = allSent
    resFinal = allSentMerge
  }
  
  
  return(resFinal)
}


# regroupSentences
# c("of_##Qatari@@_##Male@@_students in_##Government@@_schools in_##2022/2023@@",
#   "of_##Qatari@@_##Female@@_students in_##Government@@_schools in_##2022/2023@@",
#   "of_##Qatari@@_##Female@@_students in_##Private@@_schools in_##2022/2023@@",
#   "of_##Qatari@@_##Male@@_students in_##Government@@_schools in_##2021/2022@@",
#   "of_##Non-Qatari@@_##Female@@_students in_##Government@@_schools in_##2021/2022@@",
#   "of_##Non-Qatari@@_##Female@@_students in_##Private@@_schools in_##2021/2022@@")
# is regrouped in:
# "of ##Non-Qatari<OR>Qatari@@ ##Female<OR>Male@@ students in ##Government@@ schools in ##2021/2022@@"
# "of ##Qatari@@ ##Female@@ students in ##Private@@ schools in ##2021/2022@@"                         
# "of ##Non-Qatari@@ ##Female<OR>Male@@ students in ##Government@@ schools in ##2022/2023@@"          
# "of ##Non-Qatari@@ ##Female@@ students in ##Private@@ schools in ##2022/2023@@"
regroupSentences <- function(allSent,attrDelimiter){
  
  # print("---------------- regroupSentences ------------------")
  
  allSentMerge = NULL
  allSent = unique(allSent)
  
  if (length(allSent)>1){
    stop2 = FALSE
    k2 = 0
    while (stop2 == FALSE && length(allSent)>1){
      # print(paste0("=== ALL SENT ",k2," === ",allSent))
      k2 = k2 + 1 
      allSentMerge = NULL # reinit
      # remove duplicates
      refSent = allSent[1]
      candSent = allSent[2:length(allSent)] # list of candidate sentences to merge
      stop1 = FALSE
      while(stop1 == FALSE){ # still some candidate to merge
        
        cntDiff = NULL
        locDiff = NULL
        sentDiff = NULL
        
        l11 =  splitAttr1(refSent,attrDelimiter)
        
        # print(paste0("REF_SENT === ",refSent))
        # print(paste0("CAND_SENT === ",candSent))
        # print(paste0("RESULT_SENT @@@ ",allSentMerge))
         
        for (i2 in 1:length(candSent)) {
          l22 = splitAttr1(candSent[i2],attrDelimiter)
          
          cntDiff[i2] = 0
          for (ii in 1:length(l11)){
            if (l11[ii]!=l22[ii]){
              cntDiff[i2] = cntDiff[i2]+1
              locDiff[i2] = ii
              sentDiff[i2] = l22[ii]
            }
          }
        }
        l12 = NULL
        
        # find all places with single difference compared to sent1
        ind1 = which(cntDiff == 1)
        if (length(ind1)>0){
          countFreq = aggregate(locDiff[ind1], list(num=locDiff[ind1]), length)
          indMaxFreq = which.max(countFreq$x) # place with most frequent change
          indLocDiffToMerge = countFreq$num[indMaxFreq]
          indSentToMerge = ind1[locDiff[ind1] == indLocDiffToMerge]
          sentToMerge = sort(c(l11[indLocDiffToMerge],sentDiff[indSentToMerge]))
          l12 = l11
          
          # "##Female@@","##Male@@"  => "##Female<OR>Male@@"
          l12[indLocDiffToMerge] = regroupAttr(sentToMerge,attrDelimiter," or ")
          
          if (is.null(l12)) {
            newSentMerge = paste0(l11,collapse="") # add the unchanged reference sentence
          } else {
            newSentMerge = paste0(l12,collapse="") # add the reference sentence merged with other 1-diff sentences 
          }
          allSentMerge = c(allSentMerge,newSentMerge)
          
          # remove merged sentence from candidate set
          candSentNew = NULL
          for (j in 1:length(candSent)){
            if (!(j %in% indSentToMerge)) { 
              candSentNew = c(candSentNew,candSent[j])
            }
          }
          
          if (is.null(candSentNew)) { # no more sentence to test for merging 
            stop1 = TRUE
          } else if (length(candSentNew)==1){
            # add the last candidate to the set of result,
            allSentMerge = c(allSentMerge,candSentNew)
            # and stop as nothing is let to be merged with it inthe next round
            stop1 = TRUE
          } else {
            # more than one candidate sentence
            # pick the first one as the next reference
            refSent = candSentNew[1]
            candSent = candSentNew[2:length(candSentNew)]
          }
          
        } else {
          # Add the current reference to the results
          allSentMerge = c(allSentMerge,refSent)
          # print(paste0("CUR RESULT === ",allSentMerge))
          
          candSentNew = NULL
          # pick the first one as the next reference
          refSent = candSent[1]
          if (length(candSent)==1) { 
            # it was the last candidate, add it to the set of results,
            allSentMerge = c(allSentMerge,refSent)
            candSent=NULL
            # and stop as nothing is let to be merged with it inthe next round
            stop1 = TRUE
          } else{ 
            for (j in 2:length(candSent)){
              candSentNew = c(candSentNew,candSent[j])
            }
            candSent = candSentNew
          }
        }
        
      } # END INNER WHILE
      
      if (length(allSentMerge) == length(allSent)) { 
        # no merging occured, stop
        stop2 = TRUE 
      } else {
        # some merging occured, loop again on the remaining attributes
        allSent = allSentMerge
      }
      if (k2 > 5) stop2 = TRUE
      
      allSent = unique(allSent)
    } # END OUTER WHILE 
  } else {
    allSentMerge = allSent
    
  }
  
  return(allSentMerge)
}

# "in_##Flint or Port Huron@@_city" "in" "the" ",_##Michigan@@" 
# "in_##Flint or Port Huron_city in the ,_Michigan@@" 
mergeDelim <- function(lmerge,attrDelimiter){
  lm1 = gsub(attrDelimiter[1],"",lmerge)
  lm2 = gsub(attrDelimiter[2],"",lmerge)
  lm12 = gsub(attrDelimiter[2],"",lm1)
  
  if (length(lmerge)>2){
    return(paste0(c(lm2[1], lm12[2:(length(lm1)-1)],lm1[length(lm1)]),collapse = " "))
  } else if (length(lmerge)==2){
    return(paste0(lm2[1], " ", lm1[length(lm1)]))
  } else {
    return(lmerge)
  }
  
}

# Get Oxford style grouping of terms
# termList = c("a")  / c("a","b") / c("a","b","c") / c("a","b","c","d")...
# termConjunct = "or"
# return "a" / "a or b"  / "a, b, or c" / "a, b, c, or d" ...
getOxfordStyleGroup <- function(termList,termConjunct="or"){
  lstr = length(termList)
  conjspace = paste0(" ",termConjunct," ")
  if (lstr==1){ # a
    termsGrouped = termList
  } else if (lstr==2){ # a or b
    termsGrouped = paste0(termList, collapse = conjspace)
  } else { # c("a","b","c") => "a, b, or c"
    termsGrouped = paste0(paste0(paste0(termList[1:(lstr-1)], collect = ", "),collapse=""),conjspace,termList[lstr])
  }
  return(stringr::str_squish(termsGrouped))
}


##### NLQ EXPRESSION #####
geneNLQselectExpress <- function(HCTcolH = NULL,
                                 HCTrowH = NULL,
                                 exprListSQL=NULL,
                                 groupByAttrNames=NULL,
                                 orderByK=NULL,
                                 forWhich=NULL,
                                 nlPattern,
                                 allNLattrNames,
                                 simplifyNestedList){
  
  
  print("======================================================================")
  # print("HCTcolH")
  # print(HCTcolH)
  # print("HCTrowH")
  # print(HCTrowH)
  # print("exprListSQL")
  # print(exprListSQL)
  # print("groupByAttrNames")
  # print(groupByAttrNames)
  # print("orderByK")
  # print(orderByK)
  # print("forWhich")
  # print(forWhich)
  # print("nlPattern")
  # print(nlPattern)
  # print("allNLattrNames")
  # print(allNLattrNames)
  # print("----------------------------------------------------------------------")
  # several levels of a nested attributes, that can be simplified, keep only the rightmost one
  # Category    Item  =>  Item
  #     Meat    Beef      Beef
  #    Dairy  Cheese    Cheese
  #    Dairy Yoghurt   Yoghurt
  allNamesToRemove <- NULL
  for (simpNested in simplifyNestedList){
    simplifyNested = getSQLattrNames(unlist(simpNested)) # c("Category", "Item")
    # nested attributes are never split between row and col.
    
    isInSimplifyNestedAndHCTcol = simplifyNested[is.element(simplifyNested, colnames(HCTcolH))]
    if (length(isInSimplifyNestedAndHCTcol) > 1) {
      for (i in 1:(length(isInSimplifyNestedAndHCTcol) - 1)){
        nameToRemove = isInSimplifyNestedAndHCTcol[i]
        HCTcolH[nameToRemove] <- NULL
        allNamesToRemove = c(allNamesToRemove,nameToRemove)
      }
    }
    
    isInSimplifyNestedAndHCTrow = simplifyNested[is.element(simplifyNested, colnames(HCTrowH))]
    if (length(isInSimplifyNestedAndHCTrow) > 1) {
      for (i in 1:(length(isInSimplifyNestedAndHCTrow) - 1)){
        nameToRemove = isInSimplifyNestedAndHCTrow[i]
        HCTrowH[nameToRemove] <- NULL
        allNamesToRemove = c(allNamesToRemove,nameToRemove)
      }
    }
  }
  allNLattrNames <- allNLattrNames[!allNLattrNames %in% allNamesToRemove]
  # print("HCTcolH")
  # print(HCTcolH)
  # print("HCTrowH")
  # print(HCTrowH)
  # print("allNLattrNames")
  # print(allNLattrNames)
  # print("======================================================================")
  
  # HCTcolH
  # HCTrowH
  # exprListName=NULL
  # groupByAttrNames=NULL
  # orderByK=NULL
  # forWhich=NULL
  # nlPattern
  
  
  
  nlSegments = unlist(strsplit(stringr::str_squish(nlPattern),split=" "))
  
  #nlSegments = unlist(strsplit(nlPattern,split="\\$"))
  
  # identify segments containing a variable (\\$)
  # if variable is column, replace "_" by " " and $... by variable value
  # if variable is row, possibly multiple, regroup all row variable side-by-side
  #                     then for each row of HCTrowH, generate subsegment 
  nlSegmentsIsVar=nlSegments
  for (i in 1:length(nlSegments)){
    if(grepl("$", nlSegments[i], fixed=TRUE) ){ # there is a $ in the name, it is a variable
      varName3 = getAttrNamesFromPattern(nlSegments[i],c(colnames(HCTcolH),
                                               colnames(HCTrowH),
                                               "EXPR",
                                               "GROUPBY",
                                               "ORDERBYDESC",
                                               "TOPK",
                                               "OPATTR",
                                               "OPANDVAL"
      ))
      if (length(varName3)==1){
        if (varName3 %in% colnames(HCTcolH)) {
          nlSegmentsIsVar[i] = "COL"
        } else if (varName3 %in% colnames(HCTrowH)) {
          nlSegmentsIsVar[i] = "ROW"
        } else if (varName3 == "EXPR"){
          nlSegmentsIsVar[i] = "EXPR"
        } else if (varName3 == "GROUPBY"){
          nlSegmentsIsVar[i] = "GROUPBY"
        } else if (varName3 == "ORDERBYDESC" || varName3 == "TOPK"){
          nlSegmentsIsVar[i] = "ORDERBYK"
        } else if (varName3 == "OPATTR"){
          nlSegmentsIsVar[i] = "FORWHICH_START"
        } else if (varName3 == "OPANDVAL"){
          nlSegmentsIsVar[i] = "FORWHICH_END"
        } else {
          nlSegmentsIsVar[i] = "TRUE"
        }
      } else if (length(varName3)>1){
        isThereCOL = FALSE
        isThereROW = FALSE
        for (ii in 1:length(varName3)){
          if (varName3[ii] %in% colnames(HCTcolH)) isThereCOL = TRUE
          if (varName3[ii] %in% colnames(HCTrowH)) isThereROW = TRUE
        }
        if (isThereCOL && isThereROW) {
          nlSegmentsIsVar[i] = "ROWCOL"
        } else if (isThereCOL) {
          nlSegmentsIsVar[i] = "COL" 
        } else if (isThereROW) {
          nlSegmentsIsVar[i] = "ROW"
        } else {
          nlSegmentsIsVar[i] = "TRUE"
        }
      } else {
        nlSegmentsIsVar[i] = "TRUE"
      }
    } else {
      nlSegmentsIsVar[i] = "FALSE"
    }
  }
  
  # reorder segments to put adjacent all same types to be repeated 
  # ROW1 COL2 COL3 ROW4 COL5 -> ROW1 ROW4 COL2 COL3 COL5 -> ROW1 or ROW4, and COL2 or COL3 or COL5
  if (!is.null(HCTcolH)) { 
    nrowHCTcolH = nrow(HCTcolH) 
  } else {
    nrowHCTcolH = 0
  }
  if (!is.null(HCTrowH)) { 
    nrowHCTrowH = nrow(HCTrowH) 
  } else {
    nrowHCTrowH = 0
  }
  
  if (FALSE) { #(nrowHCTcolH == 1 && nrowHCTrowH == 1) {
    curNorep = NULL
    for (i in 1:length(nlSegments)){
      curType = nlSegmentsIsVar[i]
      if (curType == "COL") { 
        curVal = getVarValueNLQ(HCTcolH,nlSegments[i],allNLattrNames)
      } else if (curType == "ROW") { 
        curVal = getVarValueNLQ(HCTrowH,nlSegments[i],allNLattrNames)
      } else if (curType == "ROWCOL") { 
        curVal = getVarValueNLQ(cbind(HCTrowH,HCTcolH),nlSegments[i],allNLattrNames)
      } else if (curType == "EXPR"){
        curVal = getExprNLQ(exprListSQL,nlSegments[i])
      } else if (curType == "GROUPBY"){
        curVal = getGroupByNLQ(groupByAttrNames,nlSegments[i],allNLattrNames)
      } else if (curType == "ORDERBYK"){
        curVal = getOrderByNLQ(orderByK,nlSegments[i])
      } else if (curType == "FORWHICH_START" || curType == "FORWHICH_END"){
        curVal = getForWhichNLQ(forWhich,nlSegments[i])
      } else if (curType == "TRUE"){
        # there is a $ in pattern but no value available
        curVal = ""
      } else {
        curVal = nlSegments[i]
        
      }
      curNorep[i] = curVal
    }
    NLQstrNorep = paste0(curNorep,collapse=" ")
    nlQuestion = NLQstrNorep
    
  } else {
    # reorder segments to put adjacent all same types to be repeated 
    # ROW1 COL2 COL3 ROW4 COL5 -> ROW1 ROW4 COL2 COL3 COL5 -> ROW1 or ROW4, and COL2 or COL3 or COL5
    
    nlSegmentsSTARTval = NULL
    nlSegmentsSTARTtype = NULL
    nlSegRepCol = NULL # keep only COL attributes to group them
    nlSegRepRow = NULL # keep only ROW attributes to group them
    nlSegRepRowCol = NULL # keep only the segment containing both ROW and COL attributes (connected by "__") 
    nlSegRowColPattern = NULL # keep ROW and COL in same order as they appear in nlPattern
    nlSegmentsENDval = NULL
    nlSegmentsENDtype = NULL
    ik = 0
    ic = 0
    ir = 0
    irc = 0
    ie = 0
    irrcc = 0
    for (i in 1:(length(nlSegments)-1)){
      curSeg = nlSegments[i]
      curType = nlSegmentsIsVar[i]
      if (curType == "FALSE" || curType == "EXPR" 
          || curType == "GROUPBY" || curType == "ORDERBYK" 
          || curType == "FORWHICH_START" ) { 
        ik = ik+1
        nlSegmentsSTARTval[ik] = curSeg
        nlSegmentsSTARTtype[ik] = curType
      } else if (curType == "COL") {
        ic = ic+1
        nlSegRepCol[ic] = curSeg  
        irrcc = irrcc + 1
        nlSegRowColPattern[irrcc] = curSeg  
      } else if (curType == "ROW"){ # "in_$Language" "in_$Year"
        ir = ir + 1
        nlSegRepRow[ir] = curSeg  
        irrcc = irrcc + 1
        nlSegRowColPattern[irrcc] = curSeg  
      } else if (curType == "ROWCOL"){ # "of_$Nationality_$Gender_students" 
        irc = irc + 1
        nlSegRepRowCol[irc] = curSeg  
        irrcc = irrcc + 1
        nlSegRowColPattern[irrcc] = curSeg  
      } else if (curType == "FORWHICH_END"){
        ie = ie + 1
        nlSegmentsENDval[ie] = curSeg
        nlSegmentsENDtype[ie] = curType
      } 
    }
    
    
    
    # no repetition part (START)
    curNorepSTART = NULL
    for (i in 1:length(nlSegmentsSTARTval)){
      curType = nlSegmentsSTARTtype[i]
      if (curType == "FALSE") { 
        curVal = nlSegmentsSTARTval[i] 
      } else if (curType == "EXPR") {
        curVal = getExprNLQ(exprListSQL,nlSegmentsSTARTval[i])
      } else if (curType == "GROUPBY") {
        curVal = getGroupByNLQ(groupByAttrNames,nlSegmentsSTARTval[i],allNLattrNames)
      } else if (curType == "ORDERBYK") {
        curVal =  getOrderByNLQ(orderByK,nlSegmentsSTARTval[i])
      } else if (curType == "FORWHICH_START") {
        curVal =  getForWhichNLQ(forWhich,nlSegmentsSTARTval[i])
      }
      curNorepSTART[i] = curVal
    }
    NLQstrNorepSTART = paste0(curNorepSTART,collapse=" ")
    
    # ROW, COl repetition part (OR clause, factorize repeated tokens)
    NLQstrROWCOLrep=""
    NLQstrCOLrep=""
    NLQstrROWrep=""
    if (FALSE){ #(is.null(nlSegRepRowCol)){
      
      # COl repetition part (OR clause, factorize repeated tokens)
      NLQstrCOLrep = geneRowColUnionFromNLPattern(HCTcolH,nlSegRepCol,allNLattrNames)
      
      # ROW repetition part (OR clause, factorize repeated tokens)
      NLQstrROWrep = geneRowColUnionFromNLPattern(HCTrowH,nlSegRepRow,allNLattrNames)
      
    } else {
      # There is at least one ROWCOL, we first generate all combinations, encompassing each attribute value to ease matching
      # of_[Qatari]_[Male]_students in_[Government]_schools in_[2022/2023]
      # of_[Qatari]_[Female]_students in_[Government]_schools in_[2022/2023]
      # of_[Qatari]_[Female]_students in_[Private]_schools in_[2022/2023]
      # of_[Qatari]_[Male]_students in_[Government]_schools in_[2021/2022]
      # of_[Non-Qatari]_[Female]_students in_[Government]_schools in_[2021/2022]
      # of_[Non-Qatari]_[Female]_students in_[Private]_schools in_[2021/2022]
      
      # generate all possible combinations given row and col instances
      allSent = NULL
      
      numrowColH = 1
      numrowRowH = 1
      if (!is.null(HCTcolH)) numrowColH = nrow(HCTcolH)
      if (!is.null(HCTrowH)) numrowRowH = nrow(HCTrowH)
      
      for (ic in 1:numrowColH){
        for (ir in 1:numrowRowH){
          if (is.null(HCTcolH)) {
            HCTbind = as.data.frame(HCTrowH[ir,],stringAsFactors = FALSE)
            HCTcolnames = colnames(HCTrowH)
          } else if (is.null(HCTrowH)) {
            HCTbind = as.data.frame(HCTcolH[ic,],stringAsFactors = FALSE)
            HCTcolnames = colnames(HCTcolH)
          } else {
            HCTbind = cbind(as.data.frame(HCTcolH[ic,],stringAsFactors = FALSE),as.data.frame(HCTrowH[ir,],stringAsFactors = FALSE))
            HCTcolnames = c(colnames(HCTcolH),colnames(HCTrowH))
          }
              
          dfRow = as.data.frame(HCTbind,stringAsFactors = FALSE)
          colnames(dfRow) = HCTcolnames
          curSent = geneRowColUnionFromNLPattern(dfRow,nlSegRowColPattern,allNLattrNames,ATTR_DELIMITER)
          allSent = c(allSent,curSent)
        }
      }
      
      # Then we regroup all sentence which differ by a single Attribute
      # redo until no regrouping is possible # ATTR_DELIMITER=c("##","@@")
      allSentMerge = regroupSentences(allSent,ATTR_DELIMITER) 
      
      # make another pass grouping adjacent attributes 
      strlinkTMP = paste0(ATTR_DELIMITER[2:1],collapse = " ")
      allSentMergeClean = NULL
      for (iii in 1:length(allSentMerge)){
        allSentMergeClean[iii] = gsub(strlinkTMP,"-----", allSentMerge[iii])
      }
      allSentMerge2 = regroupSentences(allSentMergeClean,ATTR_DELIMITER) 
      allSentMerge3 = gsub("-----"," ",allSentMerge2)
      
      # print(paste0("@@@@@@@@@@@@@@@ 3 @@ ",allSentMerge3))
      allSentMerge4 = regroupSentencesLastPass(allSentMerge3,ATTR_DELIMITER)
      # print(paste0("@@@@@@@@@@@@@@@ 4 @@ ",allSentMerge4))
      
      # regroup with Oxford style
      # regroupAllSent = getOxfordStyleGroup(allSentMerge,"or") # this one works well but generate repetitive or clauses
      regroupAllSent = getOxfordStyleGroup(allSentMerge4,"or") # this one not sure of semantic validity
      
      # add back the attribute delimiters
      NLQstrROWCOLrep = gsub(ATTR_DELIMITER[2],"",gsub(ATTR_DELIMITER[1],"",regroupAllSent))
        
    }
    
    
    # no repetition part (END)
    curNorepEND = NULL
    if (length(nlSegmentsENDval)>0){
      for (i in 1:length(nlSegmentsENDval)){
        curType = nlSegmentsENDtype[i]
        if (curType == "FORWHICH_END") {
          curVal =  getForWhichNLQ(forWhich,nlSegmentsENDval[i])
        }
        curNorepEND[i] = curVal
      }
      NLQstrNorepEND = paste0(curNorepEND,collapse=" ")
    } else {
      NLQstrNorepEND = ""
    }
    
    nlQuestion = paste0(NLQstrNorepSTART," ",NLQstrCOLrep," ",NLQstrROWrep," ",NLQstrROWCOLrep," ",NLQstrNorepEND,"?")
    
  }
  
  nlQuestionClean = gsub(" \\?", "?",str_squish(nlQuestion))
  
  # remove optional string if duplicated
  # AAA of_(pollution) BBB ... pollution ==> AAA BBB ... pollution
  # AAA of_(pollution) BBB ... CCC ==> AAA of pollution BBB ... CCC
  nlQuestionClean2 = str_squish(removeOptionalDup(nlQuestionClean))
  
  # clean up blank space before comma
  nlQuestionClean3 = gsub(" ,",",",nlQuestionClean2)
  
  # clean up comma before "?":  aaa,? -> aaa?
  nlQuestionClean4 = gsub(",\\?","\\?",nlQuestionClean3)
  
  return(nlQuestionClean4)
}

# spot all options AA_A_((OPT))_B_B_B in str identified as characters in ((...))
# for each option OPT, find if another OPT exist in str
# if yes, then remove AA_A_((OPT))_B_B_B from str
# if no, then replace AA_A_((OPT))_B_B_B by "AA A OPT B B B"
removeOptionalDup <- function(nlQuestionClean){
  
  # for unit test
  # nlQuestionClean = "bloblo  AA_A_((OPT1))_B_B_B OPT1 blabla er_OPT1 bla ((OPT2))_BB bli "
  # nlQuestionClean = "What is the evolution of_(pollution) of Industrial pollution of Soil in Baltimore in Maryland?"
  
  strTMP = "XXXXX"
  strINI = nlQuestionClean
  #strINI = nlQuestionClean
  res1 = unlist(strsplit(strINI,split="\\(\\("))
  optStr = NULL
  # detect optional strings
  for (i in 1:length(res1)){
    if (grepl("\\)\\)",res1[i])) {
      res2 = unlist(strsplit(res1[i],split="\\)\\)"))
      optStr = c(optStr,res2[1])
    }
  }
  strPROCESSED = strINI
  if (length(optStr)>0){
    for (i in 1:length(optStr)){
      # for each option OPT, find if another OPT exist in str
      optStrCUR = optStr[i]
      optStrNum = sum(stringr::str_count(res1, optStrCUR))
      if (optStrNum>1) { 
        #if yes, then remove AA_A_((OPT))_B_B_B from str
        #print(paste0(optStr[i], " found ",optStrNum, " times."))
        strOPT = gsub(paste0("\\(\\(",optStrCUR,"\\)\\)"),strTMP,strPROCESSED)
        res1_1 = unlist(strsplit(strOPT,split=" "))
        strRES = ""
        for (strCUR in res1_1){
          if (!grepl(strTMP,strCUR)) strRES = paste(strRES,strCUR)
        }
        strPROCESSED = strRES
      }else{ 
        # if no, then replace AA_A_((OPT))_B_B_B by "AA A OPT B B B"
        #print(paste0(optStr[i], " found ",optStrNum, " times."))
        strOPT = strPROCESSED
        res1_2 = unlist(strsplit(strOPT,split=" "))
        strRES = ""
        for (strCUR in res1_2){
          if (!grepl(optStrCUR,strCUR)){
            strRES = paste(strRES,strCUR)
          } else {
            strCURclean = gsub("_", " ",strCUR)
            strCURclean1 = gsub("\\(", "",strCURclean)
            strCURclean2 = gsub("\\)", "",strCURclean1)
            strRES = paste(strRES,strCURclean2)
          }
        }
        strPROCESSED = strRES
      }
    }
  }
  strCLEAN = gsub("_", " ",strPROCESSED)
  return(strCLEAN)
}



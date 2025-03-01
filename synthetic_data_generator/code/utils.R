NAME_SEP = "_"  # separator replacement to allow reading/executing SQL queries on attribute names containing blank or / or . or - separators

COL_SEP = " | " # separate two columns in results of SQL and NLQ
ROW_SEP = " || " # separate two rows in results of SQL and NLQ

ATTR_DELIMITER = c("##","@@") # symbols to manage combination of attributes internally "##attr_name@@", assume attribute names contain neither "##"nor "@@" 

NUM_DECIMAL_DIGITS_REAL_FORMAT = 2 # rounding decimal numbers in HCT table values and SQL results when computing aggregate, so both match
STR_REAL_VAL_FORMAT = paste0("%.",NUM_DECIMAL_DIGITS_REAL_FORMAT,"f") ###  PRECISION USED FOR real number values generated as character strings in HCT
STR_INT_VAL_FORMAT = "%d" ###  PRECISION USED FOR integer number values generated as character strings in HCT


getSQLattrNames <- function(attrNames,name_sep=NAME_SEP){
  
  attrNames = gsub("\\.",name_sep, attrNames)
  attrNames = gsub(" ",name_sep, attrNames)
  attrNames = gsub("-",name_sep, attrNames)
  attrNames = gsub("/",name_sep, attrNames)
  
  return(attrNames)
}

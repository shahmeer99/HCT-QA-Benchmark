### Counting synthetic benchmark HCT, Q&A

library(jsonlite)

# folder containing all Q&A benchmark data
QandAfolder = "SYNTHETIC_BENCHMARK" 

# this folder shall contain all synthetic data:
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_DB.csv",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_DB.html",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_DB_NONSEM.csv",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_DB_NONSEM.html",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_HCT.csv",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_HCT.html",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_HCT_NONSEM.csv",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_HCT_NONSEM.html",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_QandA.json",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_1_QandA_NONSEM.json",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_2_DB.csv",
  # "SYNTHETIC_BENCHMARK/Evolution_of_pollution_in_percent_set1_2_DB.html"
  # ...


suffixStr = "_QandA.json" 
Lfiles = list.files(QandAfolder)
Lfilenames = Lfiles[grepl(suffixStr,Lfiles,fixed=TRUE)]

# process tables/Q/A of each root filename
nFilenames = length(Lfilenames)
baseTableName = list(1:nFilenames)
setNum = list(1:nFilenames)
numQuestions = list(1:nFilenames)
for (kk in 1:nFilenames){
  
  print(paste0(kk,"/",nFilenames," -- ", 100*kk/nFilenames,"%"))
  
  tmp = unlist(strsplit(Lfilenames[kk],"_set"))
  baseTableName[kk] = tmp[1]
  setNum[kk] = unlist(strsplit(tmp[2],suffixStr))[1]
  
  # open json and count question templates
  qanda = read_json(paste0(QandAfolder,"/",Lfilenames[kk]))
  numQuestions[kk] = length(qanda$questions)
}
# store in a dataframe all table names, set numbers, and number of questions
df = data.frame(baseTableName = unlist(baseTableName),
                setNum = unlist(setNum),
                numQuestions = unlist(numQuestions))

# now summarizing data
UbaseTableName = unique(df$baseTableName)
NumTables = NULL
NameTables = NULL
NumQuestions = NULL
for (i in 1: length(UbaseTableName)){
  ind = which(df$baseTableName == UbaseTableName[i])
  NumTables[i] = length(ind)
  NameTables[i] = UbaseTableName[i]
  NumQuestions[i] = sum(df$numQuestions[ind]) 
}
df_results = data.frame(NameTables=NameTables,
                       NumTables = NumTables,
                       NumQuestions = NumQuestions)
df_results = rbind(df_results,data.frame(NameTables="Total",
                              NumTables = sum(df_results$NumTables),
                              NumQuestions = sum(df_results$NumQuestions)))
print(df_results)

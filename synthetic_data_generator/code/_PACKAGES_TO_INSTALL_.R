# use UTF-8 encoding in RStudio to view this file there
# PACKAGES AND VERSIONS
# R version is 3.6.1
# packageVersion('ggplot2') to get the number of installed version
library(remotes)
#remotes::install_version("htmlTable",version="2.4.0") to install a specific version of a package 

# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# loaded via a namespace (and not attached):
#   [1] compiler_3.6.1 tools_3.6.1    yaml_2.3.8    

install.packages("jsonlite") # 1.8.8

                         
# pivottabler (need updated lifecycle, pillar, ellipsis, tidyselect)
install.packages("lifecycle") # 1.0.4
install.packages("pillar") # 1.9.0
install.packages("ellipsis") # 0.3.2
install.packages("tidyselect") # 1.2.0
install.packages("pivottabler") # 1.5.5

install.packages("rlang") # 1.1.3

remotes::install_version("htmlTable",version="2.4.0") # more recent version does not work with R 3.6.1


#### install.packages("psycModel") # html_to_pdf
remotes::install_version("lattice",version = "0.20") # due to R 3.6.1
install.packages('Matrix')  # 1.6.5
remotes::install_version("MASS",version = "7.3-58.0.0") # due to R 3.6.1
install.packages('nlme')    # 3.1.164
install.packages('nloptr')  # 1.2.2.2 
install.packages('ggplot2') # 3.4.4
install.packages('lavaan')  # 0.6.17
install.packages('patchwork') # 1.2.0
install.packages('psych')     # 2.4.1

#remotes::install_version('RcppEigen',version = "0.3.3.9.4") # not working 
#install.packages('lme4') # problem due to RcppEigen
install.packages("psycModel") # 0.5.0 # html_to_pdf # do not install from source, use binary

#### do not install from source, use binary
install.packages("pagedown") # 0.20
#install.packages("pdftools") # 3.0.1 # pdf to png
install.packages("magick") # 2.7.2 # pdf to png
install.packages("tidyquery") # 0.2.4
install.packages("data.table") # 1.14.0

'dtplyr', 'googledrive', 'googlesheets4' 
install.packages("dtplyr") # 1.3.1 # needed for tidyverse
install.packages("googledrive") # 2.1.1 # needed for tidyverse
install.packages("googlesheets4") # 1.1.1 # needed for tidyverse
install.packages("tidyverse") # 2.0.0

install.packages("stringr") # 1.5.1

########################
library(jsonlite)
library(lifecycle)
library(pillar)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
#library(pdftools)  # pdf to png
library(magick)  # pdf to png

# install_version("rlang",version="1.1.0")

library(RSQLite)
library(sqldf)
library(tidyquery)
library(data.table)
library(tidyverse)
library(rlang)
library(stringr)

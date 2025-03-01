
library(magick)  # pdf to png

inputFolder = "./TABLES_NEW_ONLY/"
outputFolder = "./TABLES_NEW_ONLY/"

INPUT_FILE_EXT = "_HCT.pdf"
OUTPUT_FILE_EXT = "_HCT.png"

print("COLLECT PDF SOURCES...")

### get all table files of specific table pattern
fileList = list.files(inputFolder)
ii = 0
fileListToProcess = NULL
for (ff in fileList){
  if (grepl(INPUT_FILE_EXT,ff)) {
    ii = ii + 1
    fileListToProcess[ii] = unlist(strsplit(ff,split=INPUT_FILE_EXT))
  } 
}
print(fileListToProcess)

print("GENERATE PNG from PDF")

for (filenameRoot in fileListToProcess){
  filenamePDF = paste0(inputFolder,filenameRoot,INPUT_FILE_EXT)
  filenamePNG = paste0(outputFolder,filenameRoot,OUTPUT_FILE_EXT)
  print(paste0("Processing: ",filenamePDF," --> ",filenamePNG))
  x  <- magick::image_read_pdf(filenamePDF) 
  
  defines <- c("pdf:density" = "600","png:quality" = "100", "png:compression-level" = "0")
  image_set_defines(x, defines)
  image_write(x, path = filenamePNG, format = "png")
  
  #if (filenameRoot == "Number_of_building_constructions_set359_1") break
}

# https://www.imagemagick.org/script/command-line-options.php
# https://cran.r-project.org/web/packages/magick/magick.pdf
# # Write an image
# x <- image_read("https://jeroen.github.io/images/frink.png")
# image_write(x, "frink.png")
# # Pass some properties to PNG encoder
# defines <- c("png:compression-filter" = "1", "png:compression-level" = "0")
# image_set_defines(x, defines)
# image_write(x, "frink-uncompressed.png")
# #

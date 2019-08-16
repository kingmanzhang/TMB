
#clear the work space
rm(list=ls()) #clear all variables
setwd("~/Desktop/Hackathon2019")
filelist = list.files(pattern = ".tsv")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output <- paste0(gsub("\\.tsv$", "", input), ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE)   
  setwd("~/Desktop/Hackathon2019")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("~/Desktop/Hackathon2019")
}


# df <- read.csv(file="my.large.file.csv",nrows=2000)
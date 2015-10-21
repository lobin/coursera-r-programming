
# zaladuj i odpakuj dane
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip")

# sprawdz co tam jest
list.files("specdata")


pollutantmean <- function(directory, pollutant, id = 1:33) {
  files_full <- list.files(directory, full.names=TRUE)    #get full list of files names
  files_selected=vector()
  for (i in id){
  files_selected=c(files_selected,files_full[i])  }       #extract relevant file names
  
  lista_selected=lapply(files_selected, read.csv)         #read data from relevant files to the list
  output <- do.call(rbind, lista_selected)                #create datatables from list
  mean(output[,pollutant],na.rm=TRUE)
  }

debug(pollutantmean)
undebug(pollutantmean)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", id=200:201)


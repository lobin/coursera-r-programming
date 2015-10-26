
# zaladuj i odpakuj dane
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip")

# sprawdz co tam jest
list.files("specdata")



pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)    #get full list of files names
  files_selected=vector()
  for (i in id){
  files_selected=c(files_selected,files_full[i])  }       #extract relevant file names
  
  lista_selected=lapply(files_selected, read.csv)         #read data from relevant files to the list
  output <- do.call(rbind, lista_selected)                #create dataframe from list
  mean(output[,pollutant],na.rm=TRUE)
}

complete  <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)    #get full list of files names
  files_selected=vector()
  for (i in id){
    files_selected=c(files_selected,files_full[i])  }       #extract relevant file names
  
  lista_selected=lapply(files_selected, read.csv)         #read data from relevant files to the list
  
  valid=vector()
    for (i in id) {
      valid=c(valid,sum(complete.cases(lista_selected[[i]]['sulfate'],lista_selected[[i]]['nitrate']))) #calculate valid cases for both variables
      }       
  validcases=as.data.frame(cbind(c(id),valid)) #put it to dataframe
  names(validcases)[1]='id'                    #change name of variables
  validcases
}

corrf  <- function(directory, threshold =400 ,id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)    #get full list of files names
  files_selected=vector()
  for (i in id){
    files_selected=c(files_selected,files_full[i])  }       #extract relevant file names
  
  lista_selected=lapply(files_selected, read.csv)         #read data from relevant files to the list
  
  corrout=vector()
  for (i in id) {
    if (sum(complete.cases(lista_selected[[i]]['sulfate'],lista_selected[[i]]['nitrate']))>=threshold) #calculate number of valid cases and check threshold
      {
      corrout=c(corrout,cor(x=lista_selected[[i]]['sulfate'],y=lista_selected[[i]]['nitrate'],use="pairwise.complete.obs")) #calculate correlation for valid cases
      }
      else {corrout=c(corrout,0) #if valid cases below threshold add o to result
      }
  }    
  corrout
#   validcases=as.data.frame(cbind(c(id),valid)) #put it to dataframe
#   names(validcases)[1]='id'                    #change name of variables
#   validcases
}




debug(corrf)
a=corrf("specdata", id= 1:332)


corr (c(1:10,c(20:29)))

debug(complete)
a=complete("specdata", 1:10)
a=complete("specdata", 1:332)
a
class(a)
names(a)[1]='id'
a[,1]

undebug(complete)

a=lapply(out,mean)

a=(out)
str(a)
a$sulfate

b=sum(complete.cases(a[[2]][c('sulfate','nitrate')]))
b
lapply(a[[2]]['sulfate'],sum, na.rm=TRUE)
lapply(a[[1]][c('sulfate','nitrate')],is.na)
m=lapply(a[[1]][c('sulfate','nitrate')],complete.cases)
m=lapply(a[[1]][c('sulfate','nitrate')],complete.cases(a[[1]]))
sum(m)

complete.cases(a[[1]]['sulfate'],a[[1]]['nitrate'])
as.data.frame(cbind(c(1:10),c(11:20)))

good <- sum(complete.cases(a[[1]]['sulfate'],a[[1]]['nitrate']))
sum(good)

debug(pollutantmean)
undebug(pollutantmean)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", id=200:201)


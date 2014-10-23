complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory) ) {
                stop("The directory parameter is not a valid path.")
        }
        fileNames<-list.files(directory, pattern="*.csv", full.names=TRUE)
        selectedFileNames<-fileNames[id]
        superData<-lapply(selectedFileNames, read.csv)
        superDataComplete<-lapply(superData,complete.cases)
        nobs<-lapply(superDataComplete, sum)
        output<-cbind(id,nobs)
        data.frame(output)
       
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory) ) {
                stop("The directory parameter is not a valid path.")
        }
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        if ( !pollutant == "sulfate" && !pollutant == "nitrate") {
                stop("Invalid Pollutant.  Valid options are nitrate or sulfate." )
        }
        fileNames<-list.files(directory, pattern="*.csv", full.names=TRUE)
        selectedFileNames<-fileNames[id]
        superData<-lapply(selectedFileNames, read.csv)
        superDataCombo<-do.call(rbind,superData)
        pollutantOnly<-superDataCombo[pollutant]
        #The example results only displayed to the fourth most significant digit
        signif(mean(pollutantOnly[!is.na(pollutantOnly[pollutant])]), digits=4)
}


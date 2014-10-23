corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory) ) {
                stop("The directory parameter is not a valid path.")
        }
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        completeCases<-complete(directory)
        fileNames<-list.files(directory, pattern="*.csv", full.names=TRUE)
        completeCasesWithFiles<-cbind(completeCases, fileNames)
        passes<-completeCasesWithFiles[completeCasesWithFiles["nobs"]>=threshold, 1:3]
        isFirst<-TRUE
        output<-vector('numeric')
        for (file in passes$fileNames) {
                csvFile<-read.csv( file )
                cc<-csvFile[complete.cases(csvFile),]
                output<-append(output,signif(cor(cc$nitrate, cc$sulfate), digits=4))
        }
         
        ## Return a numeric vector of correlations
        output
}
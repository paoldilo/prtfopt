loadTickersFromFile <- function()
{
    a<-read.csv("tickers.cfg",header = FALSE,stringsAsFactors = FALSE)
    tckrVector<-as.character(a[1,])
    return(tckrVector)
}

percDifference<- function(vectorQ)
{
    #computes the percentage difference from the previous day as
    # [P(t)-P(t-1)]/P(t-1)
    #View(vectorQ)
    return(-diff(vectorQ)/tail(vectorQ,length(vectorQ)-1))
}
getMainDataEx<- function(mainData)
{
    mainData2 <- data.frame(lapply(mainData[2:ncol(mainData)],mean))
    return(mainData2)
}
getMainDataVar<- function(mainData)
{
    mainData2 <- data.frame(lapply(mainData[2:ncol(mainData)],var))
    return(mainData2)
}
getMainDataSd<- function(mainData)
{
    mainData2 <- data.frame(lapply(mainData[2:ncol(mainData)],sd))
    return(mainData2)
}
getMainDataCovarianceMx <- function (mainData)
{
    #get only the data and not the Date
    mainData2 <- mainData[,2:ncol(mainData)]
    k <- ncol(mainData2) #number of variables
    n <- nrow(mainData2) #number of subjects
    
    #create means for each column
    Mx_mean <- data.frame(data=1, nrow=n) %*% as.matrix(getMainDataEx(maindata) [1,])
    
    #creates a difference matrix
    differ <- as.matrix(mainData2) - Mx_mean
    
    #creates the covariance matrix
    covMx <- (n-1)^-1*t(differ) %*% differ
    return(covMx)
}

getMainDataCorrelationMx <- function(covMx)
{
    return(cor<-cov2cor(covMx))
}
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
    return((-diff(vectorQ)*100)/tail(vectorQ,length(vectorQ)-1))
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
    Mx_mean <- matrix(data=1, nrow=n) %*% as.matrix(getMainDataEx(mainData) [1,])
    
    #creates a difference matrix
    differ <- as.matrix(mainData2) - Mx_mean
    
    #creates the covariance matrix
    covMx <- (n-1)^-1*t(differ) %*% differ
    return(covMx)
}

getMainDataCorrelationMx <- function(covMx)
{
    #return the correlation matrix
    return(cor<-cov2cor(covMx))
}

getMainDataParameters<- function(mainData)
{
  #takes a mainData data frame and returns a dataframe with:
  # first row -> expected value or mean
  # second row -> variance
  # third row -> standard deviation
  mainData2 <- rbind(getMainDataEx(mainData),getMainDataVar(mainData),getMainDataSd(mainData))
}

initWeight<- function(dimension)
{
  #initialize the weight vector with random variables
  matrix(data=runif(dimension, min=0, max=0.2),ncol = 1,nrow = dimension)
}
computeVolatility <- function(covMx,portWeight)
{
  #compute the variance value as wT*cov*w
  #compute the volatility as the square root of the variance
  variance <- t(portWeight)%*% covMx %*% portWeight
  return(sqrt(variance))
}

computePortfExp<- function(mainDataParamExpect,portWeight)
{
  #compute the portfolio expected return multiplying 
  #the expected return of the stocks for their weight
  return(mainDataParamExpect %*% t(portWeight))
}

fitnessFunction <- function(x,Expect)
{
  #x has n dimensions
}
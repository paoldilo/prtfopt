if(!exists("getQuoteTable", mode="function")) source('dwld.R')
if(!exists("percDifference", mode="function")) source('utils.R')
library(GA)
library(doParallel)
library(parallel)

getMainDataTable<- function(years=3,periodicity="D")
{
  # load tickers from file
  tckrVector<-loadTickersFromFile()
  # get the data from Yahoo
  mainData<-getQuotesDataFrame(tckrVector,years,periodicity)
  #elaborate data frame
  #mainData2<-diff(mainData[,-1])/tail(mainData[,-1],nrow(mainData)-1)
  #mainData2 <- data.frame(apply(mainData[2:ncol(mainData)],2,A))
  mainData2 <- data.frame(mainData[2:nrow(mainData),1],lapply(mainData[2:ncol(mainData)],percDifference))
  colnames(mainData2)[1]<-c("Date")
  return(mainData2)
}

computeGAport <- function(Expect,Min,Max,PopSize,Maxiter,Run)
{
  n<-11
  GA2 <- ga(type = "real-valued", 
      fitness =  fitnessFunction,... = Expect,
      min = matrix(data=Min, ncol =n,nrow=1), max = matrix(data=Max, ncol =n,nrow=1), 
      popSize = 150, maxiter = Maxiter, monitor = FALSE,
      seed = 12345, parallel = TRUE,run = Run)
}
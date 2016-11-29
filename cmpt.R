source('dwld.R')

loadTickersFromFile <- function()
{
  a<-read.csv("tickers.cfg",header = FALSE,stringsAsFactors = FALSE)
  tckrVector<-as.character(a[1,])
  return(tckrVector)
}

getMainDataTable<- function(years=3,periodicity="D")
{
  # load tickers from file
  tckrVector<-loadTickersFromFile()
  # get the data from Yahoo
  mainData<-getQuoteTable(tckrVector,years,periodicity)
  
  #elaborate data frame
  
}
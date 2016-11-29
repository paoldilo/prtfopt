
#symboli <- c("MSFT","C","MMM")


getQuoteTable<- function(symboli,years=3,periodicity="D")
{
     query_string=""
     ## make some checks to the args 
     ##check the symboli to verify this is a string
     if(is.character(symboli))evrth_fine=TRUE else evrth_fine=FALSE
     ## check for the year to verify it is not negative and not >10
     if (years >0 & years <=10) evrth_fine=TRUE else evrth_fine=FALSE
     # check for periodicity , valid values are "D,W,M" daily,weekly,monthly
     if (periodicity=="D" | periodicity=="W" | periodicity=="M") evrth_fine=TRUE else evrth_fine=FALSE
     # if something's wrong exit
     if (evrth_fine==FALSE) stop("Invalid arguments")
     ## create the date query string
     dataOggi=Sys.Date()
     YearEnd<-as.numeric(format(dataOggi,'%Y'))
     MonthEnd<-as.numeric(format(dataOggi,'%m'))-1 #months start with 0 in yahoo finance
     DayEnd<-as.numeric(format(dataOggi,'%d'))
     # go back to years parameter
     YearStart<-as.numeric(format(dataOggi,'%Y'))-years
     MonthStart<-as.numeric(format(dataOggi,'%m')) #months start with 0 in yahoo finance
     DayStart<-as.numeric(format(dataOggi,'%d'))
     query_string=paste0("&a=",MonthStart,"&b=",DayStart,"&c=",YearStart,"&d=",MonthEnd,"&e=",DayEnd,"&f=",YearEnd)
     ## add the periodicity string
     query_string=paste0(query_string,switch(periodicity,D="&g=d&ignore=.csv",M="&g=m&ignore=.csv",W="&g=w&ignore=.csv"))
     ##make the querY
     URL <- paste0("http://chart.finance.yahoo.com/table.csv?s=",symboli,query_string)
     dat <- read.csv(URL,header = TRUE,dec = ".",colClasses = c("Date","numeric","numeric","numeric","numeric","integer","numeric"))
     dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
     assign( paste0( symboli[i],"_data"), dat)
     #return the table
     return(dat)
}


getQuotesDataFrame<- function(symbolist,years=3,periodicity="D")
{
    ##check the symboli to verify this is a string
    if(is.vector(symbolist) & !is.list(symbolist) & (mode(symbolist) %in% c("character"))) evrth_fine=TRUE else evrth_fine=FALSE
    if (years >0 & years <=10) evrth_fine=TRUE else evrth_fine=FALSE
    # check for periodicity , valid values are "D,W,M" daily,weekly,monthly
    if (periodicity=="D" | periodicity=="W" | periodicity=="M") evrth_fine=TRUE else evrth_fine=FALSE
    # if something's wrong exit
    if (evrth_fine==FALSE) stop("Invalid arguments")
    
    
    for(i in seq_along(symbolist)) {
        #call the getQuoteTable function
        dt_table<- getQuoteTable(symbolist[i],years,periodicity)
        #take only the date and adjusted close columns
        
        #merge with the other tickers
    }
    #return the full dataframe
}

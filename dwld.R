
#symboli <- c("MSFT","C","MMM")


getQuotesDf<- function(symboli,years=3,periodicity="D")
{
     query_string=""
     ## make some checks to the args 
     ##check the symbols to verify this is a string vector
     if(is.vector(symboli) & !is.list(symboli) & (mode(symboli) %in% c("character"))) evrth_fine=TRUE else evrth_fine=FALSE
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
     ##make the queries
     for(i in seq_along(symboli)) {
          URL <- paste0("http://chart.finance.yahoo.com/table.csv?s=",symbols[i],query_string)
          dat <- read.csv(URL,header = TRUE,dec = ".",colClasses = c("Date","numeric","numeric","numeric","numeric","integer","numeric"))
          dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
          assign( paste0( symboli[i],"_data"), dat)
          
     }
     #return(dat)
}

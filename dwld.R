
symbols <- c("MSFT","C","MMM")


get_quotes_df<- function(symbols,years=3,periodicity="D")
{
     query_string
     ## make some checks to the args 
     ##check the symbols to verify this is a string vector
     if(is.vector(someVector) & !is.list(someVector) & (mode(symbols) %in% c("character"))) evrth_fine=TRUE else evrth_fine=FALSE
     ## check for the year to verify it is not negative and not >10
     if (years >0 & years <=10) evrth_fine=TRUE else evrth_fine=FALSE
     # check for periodicity , valid values are "D,W,M" daily,weekly,monthly
     if (periodicity=="D" | periodicity=="W" | periodicity=="M") evrth_fine=TRUE else evrth_fine=FALSE
     # if something's wrong exit
     if (evrth_fine==FALSE) stop("Invalid arguments")
     ## create the date query string
     
     ## add the periodicity string
     
     ##make the queries
     for(i in seq_along(symbols)) {
     +     URL <- paste0("http://chart.finance.yahoo.com/table.csv?s=",symbols[i],"&a=10&b=28&c=2011&d=10&e=28&f=2016&g=d&ignore=.csv")
     +     dat <- read.csv(URL)
     +     dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
     +     assign( paste0( symbols[i],"_data"), dat)
     +     dat <- NULL
     }
}

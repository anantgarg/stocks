#' Yahoo data
#' 
#' Download historical prices for a given stock from Yahoo Finance.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return dataframe with historical prices
#' @export
yahoodata <- function(ticker, from, to){
  from <- as.Date(from);
  to <- as.Date(to);
  
  args <- list(
    s = ticker,
    a = as.numeric(format(from, "%m"))-1,
    b = as.numeric(format(from, "%d")),
    c = as.numeric(format(from, "%Y")),
    d = as.numeric(format(to, "%m"))-1,
    e = as.numeric(format(to, "%d")),
    f = as.numeric(format(to, "%Y")),
    ignore = ".csv"
  );
  
  myurl <- paste("http://ichart.finance.yahoo.com/table.csv?", 
    paste(names(args), args, sep="=", collapse="&"), sep="");
    
  mydata <- tryCatch(read.csv(myurl), error=function(e){
  	stop("Failed to download data from Yahoo. Could be invalid stock. OK")
  });
  
  mydata$Date <- as.Date(mydata$Date);
  
  d <- data.frame(
  s1 = c(1,1,1,1,0,0,0,0,-1,-1,-1,-1,0,0,0,0),
  s2 = c(0,0,0,0,1,1,1,1,-1,-1,-1,-1,1,1,1,1),
  m1 = c(-1,1,0,1,-1,1,0,1,-1,1,0,1,-1,1,0,1),
  m2 = c(-1,0,1,0,-1,0,1,0,-1,0,1,0,-1,0,1,0),
  price = c(250,300,350,300,300,250,300,350,350,300,250,300,300,350,300,250),
  l1 = c(-1,0,1,1,1,1,0,-1,0,-1,1,1,1,1,-1,0),
  l2 = c(-1,1,0,0,0,0,1,-1,1,-1,0,0,0,0,-1,1),
  z1 = c(-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1),
  mc1 = c(-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,1),
  preference = c(1,3,1,2,7,9,9,8,6,7,7,5,9,7,10,8),
  stringsAsFactors = FALSE
);

fit <- lm(preference ~ s1+s2+m1+m2+price+l1+l2+z1+mc1, data=d);
return(coefficients(fit));

   
}

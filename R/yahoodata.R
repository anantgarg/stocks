#' Yahoo data
#' 
#' Download historical prices for a given stock from Yahoo Finance.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return dataframe with historical prices
#' @export
yahoodata <- function(data,formulaX,random){
  
  d <- as.data.frame(data);
 d <- data.frame(
  a0 = c(1,1,1,1,0,0,0,0,-1,-1,-1,-1,0,0,0,0),
  a1 = c(0,0,0,0,1,1,1,1,-1,-1,-1,-1,1,1,1,1),
  a2 = c(500,100,900,100,500,100,900,100,500,100,900,100,500,100,900,100),
  a3 = c(1,0,-1,0,0,1,0,-1,-1,0,1,0,0,-1,0,1),
  a4 = c(0,1,-1,1,1,0,1,-1,-1,1,0,1,1,-1,1,0),
  a5 = c(1,-1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,-1),
  a6 = c(1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1),
  a7 = c(1,-1,1,-1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1),
  preference = c(8,6,5,8,5,3,4,6,3,4,4,8,5,2,8,6) 
)
  return (d);
  
 form1 <- as.formula(formulaX);
  
fit <- lm(form1, data=d);

  return(coefficients(fit));

   
}

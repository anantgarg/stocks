#' Yahoo data
#' 
#' Download historical prices for a given stock from Yahoo Finance.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return dataframe with historical prices
#' @export
yahoodata <- function(data){
  
  d <- as.data.frame(data);
 

fit <- lm(preference ~ s1+s2+m1+m2+price+l1+l2+z1+mc1, data=d);
return(coefficients(fit));

   
}

#' Yahoo data
#' 
#' Download historical prices for a given stock from Yahoo Finance.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return dataframe with historical prices
#' @export
yahoodata <- function(data,formulaX,ra){
  
  d <- as.data.frame(data);
  
 form1 <- as.formula(formulaX);
  
fit <- lm(form1, data=d);

  return(d);  
  return(coefficients(fit));

   
}

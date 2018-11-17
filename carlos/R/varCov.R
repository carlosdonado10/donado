
#' Matriz VarCovar
#'
#' @param rentabilidad Matriz o data frame de rentabilidades. (utilizar la funcion LogReturns para mejores resultados). 
#' @param tickers Vector con los tickers de los activos que entraran a la matriz varcovar.
#'
#' @return Matriz de varianaza covarianza.
#' @export
#'
#' @examples
#' rent <- LogReturns(...)
#' S <- varCov(rent, c("AAPL","MSFT","HSY"))
varCov <- function(rentabilidad,tickers){
  M=length(tickers)
  covarianza=NULL
  for (i in 1:M){
    for(j in 1:M){
      
      
      covarianza=c(covarianza,cov(rentabilidad[,i],rentabilidad[,j]))
      
    }
  }
  S=matrix(covarianza,nrow = M,ncol=M)
  colnames(S)=tickers
  return(as.data.frame(S))
}

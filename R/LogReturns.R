#' logReturns
#' Función que devuelve una matriz con los retornos logarítmicos de activos financieros, cuyos precios entran por parámetro.
#'
#' @param precios Matriz o data frame de precios de activos. Cada activo debe estar representado por una columna.
#' @param tickers Vector de los tickers de los que se sacan las rentabilidades.
#'
#' @return Matriz con los retornos logaritmicos de los activos.
#' @export
#'
#' @examples
#' precios <- PrecioAcciones(...)
#' rentab <- logReturns(precios,c("HSY","AAPL","AMZN")) 
logReturns <- function(precios,tickers){
  
  rentabilidad=NULL
  N=length(precios[,1])
  M=length(precios[1,])
  for (i in 2:N){
    for(j in 1:M){
      rentabilidad=c(rentabilidad,log(precios[i,j]/precios[i-1,j]))
    }
  }
  
  rentabilidad=matrix(rentabilidad,nrow=length(precios[,1])-1,ncol=M,byrow = TRUE)
  rentabilidad=as.data.frame(rentabilidad)
  nomRent=paste0('logreturn ',tickers)
  colnames(rentabilidad)=c(nomRent)
  return(rentabilidad)
}
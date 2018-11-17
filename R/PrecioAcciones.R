#' Precio de Acciones
#' Retorna el precio de cierr ajustado de acciones. Estos se bajan de Yahoo Finance.
#' @param tickers Vector de tickers de los activos de los que se desea bajar la informacion.
#' @param fechaInicial Fecha a partir de la cual se descargaran los precios.
#' @param fechaFinal Fecha final de la informacion descargada.
#' @param frecuencia Fecuencia de compresion. 'daily' para precios diarios, 'monthly' para precios mensuales,'weekly' para precios semanales, etc.
#'
#'
#' @return Retorna un data frame que tiene los precios de cada activo en cada columna.
#' @export
#'
#' @examples
#' precioAcciones(tickers=c("MMM","AAPL"),fechaInicial="2018-1-30",fechaFinal="2018-8-30",frecuencia="d")
#'
PrecioAcciones <- function(tickers, fechaInicial,fechaFinal,frecuencia){

  options(warn=-1)
  mtry <- try(library(tseries),silent = T)
  dtry <- try(as.POSIXct(fechaInicial),silent = T)
  dtry2 <- try(as.POSIXct(fechaFinal),silent = T)

  if(!(inherits(dtry,'try-error')&&inherits(dtry2,'try-error'))){

  if(!inherits(mtry,'try-error')){

    numtk <- length(tickers);
    all_dat <- list(); # empty list to fill in the data
    for(i in 1:numtk){
      all_dat[[i]] <- xxx <- get.hist.quote(instrument = tickers[i], start=fechaInicial, end=fechaFinal, quote = "Adjusted", provider = "yahoo", compression = frecuencia)
    }

    precios=as.data.frame(all_dat[[1]][,1])
    for(i in 2:length(all_dat)){
      precios=cbind(precios,as.data.frame(all_dat[[i]][,1]))
    }
    colnames(precios)=paste0(rep('precio',length(tickers)),tickers)

    return(precios)

  }else{

    message("La funcion requiere el paquete tseries")

    }
  }else{
    message("La fecha se debe ingresar en el formato año-mes-dia")

    }
}


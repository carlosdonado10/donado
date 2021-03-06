% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrecioAcciones.R
\name{PrecioAcciones}
\alias{PrecioAcciones}
\title{Precio de Acciones
Retorna el precio de cierr ajustado de acciones. Estos se bajan de Yahoo Finance.}
\usage{
PrecioAcciones(tickers, fechaInicial, fechaFinal, frecuencia, tipo)
}
\arguments{
\item{tickers}{Vector de tickers de los activos de los que se desea bajar la informacion.}

\item{fechaInicial}{Fecha a partir de la cual se descargaran los precios.}

\item{fechaFinal}{Fecha final de la informacion descargada.}

\item{frecuencia}{Fecuencia de compresion. 'daily' para precios diarios, 'monthly' para precios mensuales,'weekly' para precios semanales, etc.}

\item{tipo}{}
}
\value{
Retorna un data frame que tiene los precios de cada activo en cada columna.
}
\description{
Precio de Acciones
Retorna el precio de cierr ajustado de acciones. Estos se bajan de Yahoo Finance.
}

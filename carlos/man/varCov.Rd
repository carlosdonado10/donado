% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varCov.R
\name{varCov}
\alias{varCov}
\title{Matriz VarCovar}
\usage{
varCov(rentabilidad, tickers)
}
\arguments{
\item{rentabilidad}{Matriz o data frame de rentabilidades. (utilizar la funcion LogReturns para mejores resultados).}

\item{tickers}{Vector con los tickers de los activos que entraran a la matriz varcovar.}
}
\value{
Matriz de varianaza covarianza.
}
\description{
Matriz VarCovar
}
\examples{
rent <- LogReturns(...)
S <- varCov(rent, c("AAPL","MSFT","HSY"))
}

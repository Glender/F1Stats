% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/urls_extraction.R
\name{find_circuit_in_url}
\alias{find_circuit_in_url}
\title{Extract the circuit country from a url.}
\usage{
find_circuit_in_url(url)
}
\arguments{
\item{url}{Character.}
}
\value{
Character.
}
\description{
Extract the circuit country from a url.
}
\examples{
url <- 'https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html'
circuit_name <- find_circuit_in_url(url)
print(circuit_name)
}

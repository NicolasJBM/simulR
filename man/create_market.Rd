% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_market.R
\name{create_market}
\alias{create_market}
\title{Simulate a base market over several periods based on a set of assumptions.}
\usage{
create_market(
  start = Sys.Date(),
  number_months = 60,
  base_volume = 1000,
  trend = 1/20000,
  randomness = 0.1,
  seasons = NULL
)
}
\arguments{
\item{start}{Character or date. When the the time series should start.}

\item{number_months}{Integer. How many months should the time series last.}

\item{base_volume}{Integer. Set magnitude for average daily firm demand.}

\item{trend}{Double. Linear trader across all periods.}

\item{randomness}{Double. Percentage of random variation for the demand.}

\item{seasons}{Tibble. "week", "weekday" and "coefficient" indicating the distribution.}
}
\value{
A tibble with the market size (forcast and actual) for each period.
}
\description{
Simulate a base market over several periods based on a set of assumptions.
}

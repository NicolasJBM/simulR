% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_production.R
\name{update_production}
\alias{update_production}
\title{Update information about companies' journal and census based on operations.}
\usage{
update_production(competition, simperiod, base_market)
}
\arguments{
\item{competition}{List. competitors as returned by the function pdate_expenses.}

\item{simperiod}{Character. ID of the period for which the profile holds.}

\item{base_market}{List. market based returned by the function create_case.}
}
\value{
Append updated journal and census to the competitors' parameters.
}
\description{
Update information about companies' journal and census based on operations.
}

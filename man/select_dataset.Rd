% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metacore.R
\name{select_dataset}
\alias{select_dataset}
\title{Select metacore object to single dataset}
\usage{
select_dataset(.data, dataset, simplify = FALSE, quiet = FALSE)
}
\arguments{
\item{.data}{the metacore object of dataframes}

\item{dataset}{the specific dataset to subset by}

\item{simplify}{return a single dataframe}

\item{quiet}{Option to quietly load in, this will suppress warnings, but not
errors. Expects either \code{TRUE} or \code{FALSE}. Default behaviour is \code{FALSE}.}
}
\value{
a filtered subset of the metacore object
}
\description{
Select metacore object to single dataset
}

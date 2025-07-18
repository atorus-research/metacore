% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validators.R
\name{verify_DatasetMeta}
\alias{verify_DatasetMeta}
\title{Verify that the Class Type of an object is DatasetMeta with warnings}
\usage{
verify_DatasetMeta(metacore)
}
\arguments{
\item{metacore}{An object whose class type needs to be checked.}
}
\value{
Logical: TRUE if the class type of \code{metacore} is \code{DatasetMeta},
otherwise abort with errors.
}
\description{
This function that is a wrapper to the functions \code{is_metacore} and
\code{is_DatasetMeta}.

This function is not intended to be called directly by the user. It is
used as a guard clause in many features of the \code{{metatools}} package that are
intended only to be used with the subsetted Metacore object of class type
\code{DatasetMeta}. If either of the wrapped functions return \code{FALSE }then
execution is stopped and an appropriate error message is displayed.
}
\examples{
load(metacore_example("pilot_ADaM.rda"))
adsl <- select_dataset(metacore, "ADSL", quiet = TRUE)
\dontrun{
verify_DatasetMeta("DUMMY")   # Expect error
verify_DatasetMeta(metacore)  # Expect error
}
verify_DatasetMeta(adsl)      # Expect valid, i.e., return TRUE
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structure.R
\name{s_date}
\alias{s_date}
\title{Define a Date Structure}
\usage{
s_date(format = "\%Y-\%m-\%d")
}
\arguments{
\item{format}{The date format string. This should be a single string that
specifies the expected format of the date in the JSON data. The format
should be compatible with the \code{strftime} function in R.
For example, \code{"\%Y-\%m-\%d"} for "2023-10-01".}
}
\value{
An intermediate list representing the date structure definition.
}
\description{
Creates an intermediate definition for a JSON date. This is typically used
within \code{s_map()} to define fields that should contain date values.
}

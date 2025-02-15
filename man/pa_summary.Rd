% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary}
\alias{summary}
\alias{summary.dslist}
\alias{summary.yield}
\alias{summary.veg.index}
\title{Produce result summaries of the various pacu objects}
\usage{
\method{summary}{dslist}(object, ...)

\method{summary}{yield}(object, ..., by = NULL)

\method{summary}{veg.index}(object, ..., by, fun)
}
\arguments{
\item{object}{object to be summarized}

\item{...}{additional arguments. None used currently.}

\item{by}{sf or stars object containing the geometries
within which the vegetation index values should be
summarized}

\item{fun}{a function to be applied when summarizing the
vegetation index data. For example, mean, median, max,
min.}
}
\value{
when object is of class dslist, no return value. Called for side effects.

when object is of class yield, returns an object of class data.frame

when object is of class veg.index, returns an object of class stars
}
\description{
Produce summaries for the different pacu objects
}

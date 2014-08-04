\name{inspect}
\alias{inspect}
\alias{inspect.w3c_markup_validate}
\alias{inspect.w3c_markup_validate_db}
\title{Inspect \R objects}
\description{
  Display \R objects in a convenient and informative way.
}
\usage{
inspect(x, ...)
\S3method{inspect}{w3c_markup_validate}(x, details = TRUE, ...)
\S3method{inspect}{w3c_markup_validate_db}(x, details = TRUE, full = FALSE, ...)
}
\arguments{
  \item{x}{an \R object for the generic; objects inheriting from the
    respective classes for the methods.}
  \item{details}{a logical recycled to length two indicating whether to
    display detailed information on errors and warnings, respectively,
    or a character vector with elements partially matching
    \sQuote{error} or \sQuote{warning}.}
  \item{full}{a logical indicating whether to provide information about
    validation results with no errors or warnings.}
  \item{...}{arguments to be passed to and from methods.}
}
\details{
  \code{inspect()} is a generic function.

  The methods for objects inheriting from \code{"w3c_markup_validate"}
  or \code{"w3c_markup_validate_db"} (single results of markup
  validation using \code{\link{w3c_markup_validate}}, or collections of
  such results) conveniently summarize the problems found by the
  validation service as collections of tables with columns giving the
  line, column and a description of the problem.
}

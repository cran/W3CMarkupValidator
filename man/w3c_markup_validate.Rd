\name{w3c_markup_validate}
\alias{w3c_markup_validate}
\title{Validate Markup of Web Documents using W3C Markup Validation
  Services}
\description{
  Check the markup validity of web documents in \acronym{HTML},
  \acronym{XHTML}, etc., using a W3C Markup Validation service.
}
\usage{
w3c_markup_validate(baseurl = w3c_markup_validate_baseurl(),
                    uri = NULL, file = NULL, string = NULL,
                    opts = list())
}
\arguments{
  \item{baseurl}{a character string giving the URL of the W3C Markup
    Validation service to employ.}
  \item{uri}{a character string giving the URI to validate.}
  \item{file}{a character string giving the path of a file to validate.}
  \item{string}{a character string with the markup to validate.}
  \item{opts}{a named list or \code{\link[RCurl]{curlOptions}} object
    with options to use for accessing the validation service via
    \code{\link[RCurl]{getURL}} (in case \code{uri} is given) or
    \code{\link[RCurl]{postForm}} (in case \code{file} or \code{string}
    are given).}
}
\details{
  Exactly one of \code{uri}, \code{file} or \code{string} must be given.

  Validation is then performed by using the W3C Markup Validation
  service at the given \acronym{URL}, using the (still declared
  \dQuote{experimental}) SOAP 1.2 API of such a service (see
  \url{http://validator.w3.org/docs/api.html} for more information).

  If a SOAP validation response could be obtained,
  \code{w3c_markup_validate()} returns the information in the response
  organized into an object of class \code{"w3c_markup_validate"}, which
  is a list with the following elements:
  \describe{
    \item{\code{valid}}{a logical indicating the validity of the web
      document checked (TRUE iff there were no errors)}
    \item{\code{errorcount}}{an integer giving the number of errors
      found.}
    \item{\code{errors}}{a data frame with variables \sQuote{line},
      \sQuote{col}, \sQuote{message}, \sQuote{messageid},
      \sQuote{explanation} and \sQuote{source} with the obvious
      meanings, or \code{NULL}.}
    \item{\code{warningcount}}{an integer giving the number of warnings
      found.}
    \item{\code{warnings}}{a data frame with variables as for
      \code{errors}, or \code{NULL}.}
  }

  This class has methods for \code{\link{print}} for compactly
  summarizing the results, an \code{\link{inspect}} method for inspecting
  details, and an \code{\link{as.data.frame}} method for collapsing the
  errors and warnings into a \dQuote{flat} data frame useful for further
  analyses.
}
\note{
  The validation service provided by the W3C used by default for
  validation is a shared and free resource, and the W3C asks (see
  \url{http://validator.w3.org/docs/api.html} for considerate use and
  possibly installing a local instance of the validation service: 
  excessive use of the service will be blocked.

  On Debian-based systems, a local instance can conveniently be
  installed via the system command \command{apt-get install
  w3c-markup-validator} and following the instructions for providing the
  validator as a web service.
}
\seealso{
  \code{\link{w3c_markup_validate_baseurl}} for getting and setting the
  \acronym{URL} of the validation service.
  
  \code{\link{w3c_markup_validate_db}} for combining and analyzing
  collections of single validation results.
}
\examples{
## Not much to show with this as it should validate ok:
w3c_markup_validate(uri = "http://CRAN.R-project.org")
}

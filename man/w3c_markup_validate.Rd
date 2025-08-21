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
                    opts = list(), jar = NULL)
}
\arguments{
  \item{baseurl}{a character string giving the URL of the W3C Markup
    Validation service to employ.}
  \item{uri}{a character string giving the URI to validate.}
  \item{file}{a character string giving the path of a file to validate.}
  \item{string}{a character string with the markup to validate.}
  \item{opts}{a named list with options to use for accessing the
    validation service.}
  \item{jar}{a character string giving the path to a \file{vnu.jar} for
    direct local validation.}
}
\details{
  Exactly one of \code{uri}, \code{file} or \code{string} must be given.

  Validation is then (unless \code{jar} is given) performed using the
  \emph{new} W3C Markup Validation service at the given \acronym{URL},
  as appropriate for programmatic checking of modern HTML (i.e., HTML5)
  documents.  See \url{https://validator.w3.org/docs/api.html} for more
  information.

  (Versions of \pkg{W3CMarkupValidator} up to 0.1-7 used the obsolete
  SOAP 1.2 API for the Markup Validator, which cannot handle HTML5.)

  If the validation requests was successful (i.e., a 200 OK response
  status was returned), \code{w3c_markup_validate()} returns the
  information in the response organized into an object of class
  \code{"w3c_markup_validate"}, which is a data frame with with rows
  giving the diagnostic messages and the following variables:
  \describe{
    \item{\code{type}}{a character string giving the type of the
      message: one of \code{"info"}, \code{"error"} or
      \code{"document-error"}.} 
    \item{\code{subType}}{a character string giving the subtype of the
      message.  For type \code{"info"} this can be \code{"warning"} or
      missing; for type \code{"error"} this can be \code{"fatal"} or
      missing.} 
    \item{\code{firstLine}, \code{firstColumn}, \code{lastLine},
      \code{lastColumn}}{integers indicating the range of source code
      associated with the message.}
    \item{\code{message}}{a character string giving the diagnostic
      message text.} 
    \item{\code{extract}}{a character string representing an extract of
      the document source from around the point in source designated for
      the message by the line and column numbers.}
  }

  Alternatively, one can also use the underling Nu HTML checker directly
  (via invoking the Java interpreter) using a local \file{vnu.jar} JAR
  file by giving the path to this file in the \code{jar} argument.
  See \url{https://validator.github.io/validator/} for more information.
  Using the latest version of \file{vnu.jar} from
  \url{https://github.com/validator/validator/releases/tag/latest} works
  best.  

  This class has methods for \code{\link{print}} for compactly
  summarizing the results, and an \code{\link{inspect}} method for
  inspecting details.
}
\note{
  The validation service provided by the W3C used by default for
  validation is a shared and free resource, and the W3C asks (see
  \url{https://validator.w3.org/docs/api.html}) for considerate use and
  possibly installing a local instance of the validation service: 
  excessive use of the service will be blocked.  See
  \url{https://validator.w3.org/docs/install.html} for setting up a
  local service.

  On Debian-based systems, for a long time a local instance could
  conveniently be installed via the system command
  \command{apt-get install w3c-markup-validator}
  and following the instructions for providing the validator as a web
  service.  Unfortunately, as of 2025-08 this package is orphaned and
  still provides the old SOAP service which cannot handle modern HTML
  documents. 

  One can use the environment variable
  \env{W3C_MARKUP_VALIDATOR_BASEURL} to specify the service to be
  employed by default.
}
\seealso{
  \code{\link{w3c_markup_validate_baseurl}} for getting and setting the
  \acronym{URL} of the validation service.
  
  \code{\link{w3c_markup_validate_db}} for combining and analyzing
  collections of single validation results.
}
\examples{
## Not much to show with this as it should validate ok
## (provided that the validation service is accessible):
tryCatch(w3c_markup_validate(uri = "https://CRAN.R-project.org"),
         error = identity)
}

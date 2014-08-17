\name{w3c_markup_validate_baseurl}
\alias{w3c_markup_validate_baseurl}
\title{URL of W3C Markup Validation Service}
\description{
  Get or set the \acronym{URL} of the W3C Markup Validation service to
  employ.
}
\usage{
w3c_markup_validate_baseurl(new)
}
\arguments{
  \item{new}{a character string with the \acronym{URL} of the the W3C
    Markup Validation service to employ, or \code{NULL} indicating to
    use the W3C service as specified by the environment variable
    \env{W3C_MARKUP_VALIDATOR_BASEURL}, or if this is unset, 
    at \url{http://validator.w3.org/check}.
  }
}
\details{
  If no argument is given, the current \acronym{URL} is returned.
  Otherwise, the \acronym{URL} is set to the given one or (if \code{new}
  is \code{NULL}) the default one.
}

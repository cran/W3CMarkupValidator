\name{w3c_markup_validate_db}
\alias{w3c_markup_validate_db}
\alias{w3c_markup_validate_files}
\alias{w3c_markup_validate_uris}
\title{Collections of W3C Markup Validation Results}
\description{
  Create and manipulate collections of W3C markup validation results.
}
\usage{
w3c_markup_validate_db(x, names = NULL)

w3c_markup_validate_files(files,
                          baseurl = w3c_markup_validate_baseurl(),
                          opts = list(),
                          jar = NULL,
                          verbose = interactive(),
                          Ncpus = getOption("Ncpus", 1L))

w3c_markup_validate_uris(uris,
                         baseurl = w3c_markup_validate_baseurl(),
                         opts = list(),
                         jar = NULL,
                         verbose = interactive(),
                         Ncpus = getOption("Ncpus", 1L))
}
\arguments{
  \item{x}{a list of \code{\link{w3c_markup_validate}} results.}
  \item{names}{a character vector of names for the elements in
    \code{x}, or \code{NULL} (default), indicating to use the names of
    \code{x} or to auto-generate names if these are \code{NULL}.}
  \item{files}{a character vector giving the names of files to validate.}
  \item{uris}{a character vector giving \acronym{URI}s to validate.}
  \item{baseurl}{a character string giving the \acronym{URL} of the W3C
    Markup Validation service to employ.}
  \item{opts}{see \code{\link{w3c_markup_validate}}.}
  \item{jar}{see \code{\link{w3c_markup_validate}}.}
  \item{verbose}{a logical indicating if some \dQuote{progress report}
    should be given.} 
  \item{Ncpus}{the number of parallel processes to use for validating in
    parallel.} 
}
\details{
  \code{w3c_markup_validate_db()} creates a db (data base) of
  \code{\link{w3c_markup_validate}} results as a list of these results
  with class \code{"w3c_markup_validate_db"}.  This class has methods
  for \code{\link{print}} and \code{\link{c}} for compactly summarizing
  and combining results, an \code{\link{inspect}} method for inspecting
  details, and an \code{\link{as.data.frame}} method for collapsing the
  errors and warnings into a \dQuote{flat} data frame useful for further
  analyses.

  \code{w3c_markup_validate_files()} and
  \code{w3c_markup_validate_uris()} validate the markup in the given
  files or URIs, with results combined into such results db objects.
  For files or URIs for which validation failed (which can happen for
  example when these contain characters invalid in SGML), the
  corresponding error condition objects are gathered into the
  \code{"failures"} attribute of the results db returned.
}
\seealso{
  \code{\link{w3c_markup_validate_baseurl}} for getting and setting the
  \acronym{URL} of the validation service.
}
\examples{
## Test files provided with this package:
dir <- system.file("examples", package = "W3CMarkupValidator")
files <- Sys.glob(file.path(dir, "*.html"))
if(!startsWith(w3c_markup_validate_baseurl(),
               "https://validator.w3.org")) {
    ## Validate.
    results <- w3c_markup_validate_files(files)
    results
    ## In case of failures, inspect the error messages:
    lapply(attr(results, "failures"), conditionMessage)
    ## Inspect validation results:
    inspect(results)
    inspect(results, full = TRUE)
    ## Turn results into a data frame:
    df <- as.data.frame(results)
    ## Tabulate error messages:
    table(substring(df$message, 1L, 60L))
    ## Inspect a particular set of error messages:
    df[startsWith(df$message, "Unclosed element"), ]
    ## Conveniently view the full records (modulo HTML markup):
    write.dcf(df)
}
}

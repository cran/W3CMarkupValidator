use("curl")

Sys.unsetenv("W3C_MARKUP_VALIDATOR_BASEURL")

w3c_markup_validate_baseurl_default <-
function()
    Sys.getenv("W3C_MARKUP_VALIDATOR_BASEURL",
               "https://validator.w3.org/nu/?")

w3c_markup_validate_baseurl <-
local({
    baseurl <- w3c_markup_validate_baseurl_default()
    function(new) {
        if(missing(new))
            baseurl
        else {
            baseurl <<- if(is.null(new))
                w3c_markup_validate_baseurl_default()
            else
                new
        }
    }
})

w3c_markup_validate <-
function(baseurl = w3c_markup_validate_baseurl(),
         uri = NULL, file = NULL, string = NULL,
         opts = list(), jar = NULL)
{
    if(!is.null(jar))
        w3c_markup_validate_via_jar(uri, file, string,
                                    jar)
    else
        w3c_markup_validate_via_web(uri, file, string,
                                    baseurl, opts)
}

w3c_markup_validate_via_web <-
function(uri = NULL, file = NULL, string = NULL,
         baseurl = w3c_markup_validate_baseurl(),
         opts = list())
{
    ## Be nice and add the question mark at the end if not given.
    if(!endsWith(baseurl, "?"))
        baseurl <- paste0(baseurl, "?")

    res <- if(!is.null(uri)) {
               h <- new_handle()
               handle_setopt(h, .list = opts)
               curl_fetch_memory(sprintf("%sdoc=%s&out=json", baseurl,
                                         URLencode(uri)),
                                 handle = h)
           } else {
               html <- if(!is.null(file)) {
                           readBin(file, raw(), file.info(file)$size)
                       } else if(!is.null(string)) {
                           charToRaw(string)
                       } else
                           stop("You must specify one of 'uri', 'file' or 'string'.")
               h <- new_handle(postfields = html,
                               httpheader = "Content-Type: text/html")
               handle_setopt(h, .list = opts)
               curl_fetch_memory(sprintf("%sout=json", baseurl),
                                 handle = h)
           }
    
    status <- res$status_code
    if((as.integer(status) %/% 100) != 2L)
        stop("Validation request failed")

    w3c_markup_validate_results_from_JSON(rawToChar(res$content))
}
    
w3c_markup_validate_via_jar <-
function(uri = NULL, file = NULL, string = NULL, jar)
{
    if(!is.character(jar) ||
       length(jar) != 1L ||
       !file.exists(jar))
        stop("Argument 'jar' must give the file path to 'vnu.jar'.")

    java <- tools::Rcmd(c("config", "JAVA"), stdout = TRUE)
    if(!nzchar(java))
        java <- Sys.which("java")
    if(!nzchar(java))
        stop("Cannot find a Java executable.")

    if(is.null(file)) {
        file <- tempfile()
        on.exit(unlink(file))
        if(!is.null(uri))
            utils::download.file(uri, file, quiet = TRUE)
        else if(!is.null(string))
            writeLines(string, file, useBytes = TRUE)
        else
            stop("You must specify one of 'uri', 'file' or 'string'.")
    }

    txt <- system2(java,
                   c("-jar", jar, "--stdout", "--format json", file),
                   stdout = TRUE)

    w3c_markup_validate_results_from_JSON(txt)
}

w3c_markup_validate_results_from_JSON <-
function(txt)    
{
    out <- fromJSON(txt)$messages

    ## The docs at <https://validator.w3.org/docs/api.html> say that for
    ## the messages object only 'type' is mandatory, and if 'firstLine'
    ## is missing it is assumed to have the same value as 'lastLine'.

    n <- NROW(out)    
    for(k in setdiff(c("lastLine", "lastColumn", "firstColumn"),
                     names(out)))
        out[[k]] <- rep_len(NA_integer_, n)
    if("firstLine" %notin% names(out))
        out$firstLine <- out$lastLine
    for(k in setdiff(c("message", "extract", "subType"),
                     names(out)))
        out[[k]] <- rep_len(NA_character_, n)

    out <- out[c("type", "subType",
                 "firstLine", "firstColumn", "lastLine", "lastColumn",
                 "message", "extract")]

    class(out) <- c("w3c_markup_validate", class(out))

    out
}
w3c_markup_validate_results_pos_by_cat <-
function(x)
{
    i <- x$type == "info"
    j <- !is.na(s <- x$subType) & (s == "warning")
    s <- seq_along(i)
    list(errors = s[!i], warnings = s[i & j], infos = s[i & !j])
}

print.w3c_markup_validate <-
function(x, ...)
{
    writeLines(format(x, details = FALSE))
    invisible(x)
}

format.w3c_markup_validate <-
function(x, details = TRUE, ...)
{
    fmt <- function(m) {
        sprintf("  %s %s  %s",
                format(c("line", m$firstLine), justify = "right"),
                format(c(" col", m$firstColumn), justify = "right"),
                c("message",
                  gsub("[[:space:]]*\n[[:space:]]*", "  ", m$message)))
                  
    }

    p <- w3c_markup_validate_results_pos_by_cat(x)
    n <- lengths(p)
    y <- sprintf("Valid: %s (%s)",
                 n[1L] + n[2L] == 0,
                 paste(names(n), n, sep = ": ", collapse = ", "))

    details <- if(is.character(details)) {
        !is.na(pmatch(c("e", "w", "i"), details))
    } else
        rep_len(details, 3L)

    if(details[1L] && n[1L])
        y <- c(y, "Errors:", fmt(x[p$errors, ]))
    if(details[2L] && n[2L])
        y <- c(y, "Warnings:", fmt(x[p$warnings, ]))
    if(details[3L] && n[3L])
        y <- c(y, "Infos:", fmt(x[p$infos, ]))

    y
}

as.data.frame.w3c_markup_validate <-
function(x, row.names = NULL, optional = FALSE, ...)
    x

w3c_markup_validate_db <-
function(x, names = NULL)
{
    if(inherits(x, "w3c_markup_validate")) {
        x <- list(x)
    }
    if(is.null(names)) {
        names <- names(x)
        if(is.null(names))
            names <- as.character(seq_along(x))
    }
    names(x) <- names
    ## Better put on the names first: otherwise handling failures is
    ## more tricky.
    ind <- vapply(x, inherits, NA, "error")
    if(any(ind)) {
        failures <- x[ind]
        x <- x[!ind]
        attr(x, "failures") <- failures
    }
    if(!all(vapply(x, inherits, NA, "w3c_markup_validate")))
        stop("All elements must be w3c_markup_validate results.")
    class(x) <- "w3c_markup_validate_db"
    x
}

`[.w3c_markup_validate_db` <-
function(x, i)
{
    y <- NextMethod("[")
    class(y) <- class(x)
    y
}

print.w3c_markup_validate_db <-
function(x, ...)
{
    p <- lapply(x, w3c_markup_validate_results_pos_by_cat)
    n <- do.call(rbind, lapply(p, lengths))
    writeLines(sprintf("Valid: %d out of %d (%s)",
                       sum(n[, 1L] + n[, 2L]) == 0,
                       length(x),
                       paste(colnames(n), colSums(n),
                             sep = ": ", collapse = ", ")))
    if(n <- length(attr(x, "failures"))) {
        writeLines(sprintf("Failures: %d", n))
    }
    invisible(x)
}

as.data.frame.w3c_markup_validate_db <-
function(x, row.names = NULL, optional = FALSE, ...)
{
    y <- lapply(x, as.data.frame)
    lens <- sapply(y, NROW)
    do.call(rbind,
            Map(cbind,
                name = Map(rep.int, names(x), lens),
                y,
                stringsAsFactors = FALSE))
}

c.w3c_markup_validate_db <-
function(..., recursive = FALSE)
{
    args <- list(...)
    ## Could check for all elements to inherit from
    ## w3c_markup_validate_db.
    ## Note that the following does not necessarily give unique names:
    ## we could ensure these be prefixing with (possibly generated)
    ## argument names.
    w3c_markup_validate_db(unlist(args, recursive = FALSE),
                           unlist(lapply(args, names)))
}

w3c_markup_validate_files <-
function(files, 
         baseurl = w3c_markup_validate_baseurl(), opts = list(),
         jar = NULL,
         verbose = interactive(),
         Ncpus = getOption("Ncpus", 1L)) {
    FUN <- function(f)
        w3c_markup_validate(baseurl = baseurl,
                            file = f,
                            opts = opts,
                            jar = jar)
    results <-
        w3c_markup_validate_parLapply(files, FUN,
                                      verbose = verbose,
                                      Ncpus = Ncpus)
    w3c_markup_validate_db(results, files)
}

w3c_markup_validate_uris <-
function(uris, 
         baseurl = w3c_markup_validate_baseurl(), opts = list(),
         jar = NULL,
         verbose = interactive(),
         Ncpus = getOption("Ncpus", 1L)) {
    FUN <- function(u)
        w3c_markup_validate(baseurl = baseurl,
                            uri = u,
                            opts = opts,
                            jar = jar)
    results <-
        w3c_markup_validate_parLapply(uris, FUN,
                                      verbose = verbose,
                                      Ncpus = Ncpus)
    w3c_markup_validate_db(results, uris)
}

inspect <-
function(x, ...)
    UseMethod("inspect")

inspect.w3c_markup_validate <-
function(x, details = TRUE, ...)
{
    writeLines(format(x, details = details))
}

inspect.w3c_markup_validate_db <-
function(x, details = TRUE, full = FALSE, ...)
{
    if(!full) {
        p <- lapply(x, w3c_markup_validate_results_pos_by_cat)
        n <- do.call(rbind, lapply(p, lengths))
        x <- x[n[, 1L] + n[, 2L] > 0]
    }
    nmx <- names(x)
    writeLines(paste(unlist(Map(function(h, b)
                                paste(c(paste0(h, ":"),
                                        format(b, details = details)),
                                      collapse = "\n  "),
                                nmx, x)),
                     collapse = "\n\n"))
}

w3c_markup_validate_parLapply <-
function(X, FUN, ...,
         verbose = interactive(),
         Ncpus = getOption("Ncpus", 1L))
{
    stopifnot(is.character(X))
    
    one <- function(e) {
        if(verbose)
            message(sprintf("processing %s", e))
        tryCatch(FUN(e, ...), error = identity)
    }

    if(Ncpus > 1L) {
        if(.Platform$OS.type != "windows") {
            out <- parallel::mclapply(X, one, mc.cores = Ncpus)
        } else {
            cl <- parallel::makeCluster(Ncpus)
            args <- list(FUN, ...)      # Eval promises.
            out <- parallel::parLapply(cl, X, one)
            parallel::stopCluster(cl)
        }
    } else {
        out <- lapply(X, one)
    }

    names(out) <- X
    out
}

`%notin%` <-
function(x, y)
    is.na(match(x, y))


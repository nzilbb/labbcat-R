#' \packageTitle{nzilbb.labbcat}
#'
#' \packageDescription{nzilbb.labbcat}
#'
#' \packageDESCRIPTION{nzilbb.labbcat}
#' \packageIndices{nzilbb.labbcat}
#' 
#' 'LaBB-CAT' is a web-based language corpus management system and this
#' package provides access to data stored in a 'LaBB-CAT' instance.
#' You must have at least version 20210601.1528 'LaBB-CAT' to use
#' this package.
#' 
#' @docType package
#' @keywords package
#' @name nzilbb.labbcat
#' @author \packageAuthor{nzilbb.labbcat}
#' @references
#' \cite{Robert Fromont and Jennifer Hay, "{ONZE Miner}: the development of a browser-based research tool", 2008}
#' \cite{Robert Fromont, "Toward a format-neutral annotation store", 2017}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get the phonemic transcriptions for the matches
#' phonemes <- getMatchLabels(labbcat.url, results$MatchId, "phonemes")
#'
#' ## Get sound fragments for the matches
#' wav.files <- getSoundFragments(labbcat.url, results$Transcript, results$Line, results$LineEnd)
#' }
#' 
NULL

### Internal variables:

## minimum version of LaBB-CAT required:
.min.labbcat.version <- "20210601.1528"
.user.agent <- paste("labbcat-R", packageVersion("nzilbb.labbcat"), sep="/")

### Internal functions:

## prompt for password in RStudio, falling back to terminal if we're not in RStudio
get.hidden.input <- function(prompt) {
    return(tryCatch({
        ## try using the RStudio API for hidden input
        rstudioapi::askForPassword(prompt)
    }, error = function(e) {
        ## fall back to 
        readline(paste("WARNING: Input will be visible -", prompt))
    }))
}

## encode a parameter value for inclusion in the URL
enc <- function(value) {
    return(
        stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(
          URLencode(value),"\\+","%2B"),":","%3A"),"\\[","%5B"),"\\]","%5D"),"#","%23"),"&&","%26%26"))
}

## build a store call URL 
buildUrl <- function(labbcat.url, call, parameters = NULL) {
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")
    url <- paste("store?call=", call, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    url <- paste(labbcat.url, url, sep="")
    return(url)
}

## make an HTTP GET request to the store URL, asking for credentials if required
store.get <- function(labbcat.url, call, parameters = NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste("api/store/", call, "?", sep="")
    if (!is.null(parameters)) {
        mapply(function(name, value) {
            url <<- paste(url, "&", name, "=", value, sep="")
        }, names(parameters), parameters)
    } # there are parameters
    url <- enc(url)
    url <- paste(labbcat.url, url, sep="")
    
    ## attempt the request
    resp <- httr::GET(url,
                      httr::add_headers("User-Agent" = .user.agent),
                      httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)))
    ## check we don't need credentials
    if (httr::status_code(resp) == 401 && interactive()) {
        ## ask for username and password
        instance.name <- httr::headers(resp)['www-authenticate']
        if (!is.null(instance.name)) {
            ## something like 'Basic realm="Demo LaBB-CAT"'
            instance.name <- stringr::str_replace(instance.name, "^Basic realm=\"", "")
            instance.name <- stringr::str_replace(instance.name, "\"$", "")
        } else {
            instance.name <- "LaBB-CAT"
        }

        ## loop trying until success, or they cancel out
        repeat {
            error <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means everything OK
            if (is.null(error)) break
            ## "Version mismatch" means success, but wrong LaBB-CAT version
            if (grepl("version", error, ignore.case=T)) {
                print(error)
                return(NULL)
            }
        } ## next try
        
        ## and try again
        return(store.get(labbcat.url, call, parameters))
    } else {
        return(resp)
    }
}
## make an HTTP GET request to the thread URL, asking for credentials if required
thread.get <- function(labbcat.url, threadId) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, "thread?threadId=", threadId, sep="")
    
    ## attempt the request
    resp <- httr::GET(url,
                      httr::add_headers("User-Agent" = .user.agent),
                      httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)))
    ## check we don't need credentials
    if (httr::status_code(resp) == 401 && interactive()) {
        ## ask for username and password
        instance.name <- httr::headers(resp)['www-authenticate']
        if (!is.null(instance.name)) {
            ## something like 'Basic realm="Demo LaBB-CAT"'
            instance.name <- stringr::str_replace(instance.name, "^Basic realm=\"", "")
            instance.name <- stringr::str_replace(instance.name, "\"$", "")
        } else {
            instance.name <- "LaBB-CAT"
        }

        ## loop trying until success, or they cancel out
        repeat {
            error <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means everything OK
            if (is.null(error)) break
            ## "Version mismatch" means success, but wrong LaBB-CAT version
            if (grepl("version", error, ignore.case=T)) {
                print(error)
                return(NULL)
            }
        } ## next try
        
        ## and try again
        return(thread.get(labbcat.url, threadId))
    } else {
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            return()
        } else {
            resp.json <- jsonlite::fromJSON(resp.content)
            for (error in resp.json$errors) print(error)
            return(resp.json$model)
        }
    }
}
## make an HTTP GET request, asking for credentials if required
http.get <- function(labbcat.url, path, parameters = NULL, content.type = "application/json", file.name = NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(path, "?", sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            for (parameter in parameters[name]) {
                for (value in parameter) {
                    url <- paste(url, "&", name, "=", value, sep="")
                } # next value
            } # next elements
        } # next parameter
    } # there are parameters
    url <- enc(url)
    url <- paste(labbcat.url, url, sep="")
    
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::GET(url,
                          httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                          httr::add_headers("Accepts" = content.type),
                          httr::add_headers("User-Agent" = .user.agent))
    } else {
        resp <- httr::GET(url,
                          httr::write_disk(file.name, overwrite=TRUE),
                          httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                          httr::add_headers("Accepts" = content.type),
                          httr::add_headers("User-Agent" = .user.agent))
    }
    ## check we don't need credentials
    if (httr::status_code(resp) == 401 && interactive()) {
        ## ask for username and password
        instance.name <- httr::headers(resp)['www-authenticate']
        if (!is.null(instance.name)) {
            ## something like 'Basic realm="Demo LaBB-CAT"'
            instance.name <- stringr::str_replace(instance.name, "^Basic realm=\"", "")
            instance.name <- stringr::str_replace(instance.name, "\"$", "")
        } else {
            instance.name <- "LaBB-CAT"
        }

        ## loop trying until success, or they cancel out
        repeat {
            error <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means everything OK
            if (is.null(error)) break
            ## "Version mismatch" means success, but wrong LaBB-CAT version
            if (grepl("version", error, ignore.case=T)) {
                print(error)
                return(NULL)
            }
        } ## next try
        
        ## and try again
        return(http.get(labbcat.url, path, parameters, content.type, file.name))
    } else {
        return(resp)
    }
}

## make an HTTP POST request, asking for credentials if required
http.post <- function(labbcat.url, path, parameters, file.name=NULL) {
    
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::POST(url,
                           httr::add_headers("User-Agent" = .user.agent),
                           httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                           body = parameters, encode = "form")
    } else {
        resp <- httr::POST(url,
                           httr::write_disk(file.name, overwrite=TRUE),
                           httr::add_headers("User-Agent" = .user.agent),
                           httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                           body = parameters, encode = "form")
    }
    ## check we don't need credentials
    if (httr::status_code(resp) == 401 && interactive()) {
        ## ask for username and password
        instance.name <- httr::headers(resp)['www-authenticate']
        if (!is.null(instance.name)) {
            ## something like 'Basic realm="Demo LaBB-CAT"'
            instance.name <- stringr::str_replace(instance.name, "^Basic realm=\"", "")
            instance.name <- stringr::str_replace(instance.name, "\"$", "")
        } else {
            instance.name <- "LaBB-CAT"
        }

        ## loop trying until success, or they cancel out
        repeat {
            error <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means everything OK
            if (is.null(error)) break
            ## "Version mismatch" means success, but wrong LaBB-CAT version
            if (grepl("version", error, ignore.case=T)) {
                print(error)
                return(NULL)
            }
        } ## next try
        
        ## and try again
        return(http.post(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

## make an HTTP POST request, asking for credentials if required
http.post.multipart <- function(labbcat.url, path, parameters, file.name=NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::POST(url,
                           httr::add_headers("User-Agent" = .user.agent),
                           httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                           body = parameters, encode = "multipart")
    } else {
        resp <- httr::POST(url,
                           httr::write_disk(file.name, overwrite=TRUE),
                           httr::add_headers("User-Agent" = .user.agent),
                           httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                           body = parameters, encode = "multipart")
    }
    ## check we don't need credentials
    if (httr::status_code(resp) == 401 && interactive()) {
        ## ask for username and password
        instance.name <- httr::headers(resp)['www-authenticate']
        if (!is.null(instance.name)) {
            ## something like 'Basic realm="Demo LaBB-CAT"'
            instance.name <- stringr::str_replace(instance.name, "^Basic realm=\"", "")
            instance.name <- stringr::str_replace(instance.name, "\"$", "")
        } else {
            instance.name <- "LaBB-CAT"
        }

        ## loop trying until success, or they cancel out
        repeat {
            error <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means everything OK
            if (is.null(error)) break
            ## "Version mismatch" means success, but wrong LaBB-CAT version
            if (grepl("version", error, ignore.case=T)) {
                print(error)
                return(NULL)
            }
        } ## next try
        
        ## and try again
        return(http.post.multipart(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

#' \packageTitle{nzilbb.labbcat}
#'
#' \packageDescription{nzilbb.labbcat}
#'
#' \packageDESCRIPTION{nzilbb.labbcat}
#' \packageIndices{nzilbb.labbcat}
#' 
#' 'LaBB-CAT' is a web-based language corpus management system and this
#' package provides access to data stored in a 'LaBB-CAT' instance.
#' You must have at least version 20230818.1400 'LaBB-CAT' to use
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
.min.labbcat.version <- "20230224.1731"
.user.agent <- paste("labbcat-R", packageVersion("nzilbb.labbcat"), sep="/")

### Internal functions:

#' prompt for password in RStudio, falling back to terminal if we're not in RStudio
#' @param prompt Prompt to display to user
#' @return The usre's response
#' @noRd
get.hidden.input <- function(prompt) {
    return(tryCatch({
        ## try using the RStudio API for hidden input
        rstudioapi::askForPassword(prompt)
    }, error = function(e) {
        ## fall back to 
        readline(paste("WARNING: Input will be visible -", prompt))
    }))
}

#' Encode a parameter value for inclusion in the URL
#' @param value Value to encode.
#' @return URL-encoded value.
#' @noRd
enc <- function(value) {
    return(
        stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(
          URLencode(value),"\\+","%2B"),":","%3A"),"\\[","%5B"),"\\]","%5D"),"#","%23"),"&&","%26%26"))
}

#' build a store call URL 
#' @param labbcat.url URL to LaBB-CAT
#' @param call Graph store API enpoint
#' @param parameters Request parameters
#' @return URL
#' @noRd
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

#' make an HTTP GET request to the store URL, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT
#' @param call Graph store API enpoint
#' @param parameters Request parameters
#' @return response to request
#' @noRd
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
#' make an HTTP GET request to the thread URL, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT
#' @param threadId Server-side task ID
#' @param parameters Request parameters
#' @return task model returned by request
#' @noRd
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
#' make an HTTP GET request, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT.
#' @param path Endpoint path.
#' @param parameters Request parameters.
#' @param content.type Content (MIME) for encoding of response.
#' @param file.name Name of file to save response to, or NULL to not save to a fil.
#' @return Response object.
#' @noRd
http.get <- function(labbcat.url, path, parameters = NULL, content.type = "application/json", file.name = NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- path
    if (!is.null(parameters)) {
        url <- paste(path, "?", sep="")
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

#' make an HTTP POST request, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT.
#' @param path Endpoint path.
#' @param parameters Request parameters.
#' @param file.name Name of file to save response to, or NULL to not save to a fil.
#' @return Response object.
#' @noRd
http.post <- function(labbcat.url, path, parameters, file.name=NULL) {
    
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::POST(url,
                           ## httr::verbose(),
                           httr::add_headers("User-Agent" = .user.agent),
                           httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                           body = parameters, encode = "form")
    } else {
        resp <- httr::POST(url,
                           ## httr::verbose(),
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

#' make an HTTP PUT request, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT.
#' @param path Endpoint path.
#' @param parameters Request parameters.
#' @param file.name Name of file to save response to, or NULL to not save to a fil.
#' @return Response object.
#' @noRd
http.put <- function(labbcat.url, path, parameters, file.name=NULL) {
    
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    ## print(url)
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::PUT(url,
                          ## httr::verbose(),
                          httr::add_headers("User-Agent" = .user.agent),
                          httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)),
                          body = parameters, encode = "form")
    } else {
        resp <- httr::PUT(url,
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
        return(http.put(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

#' make an HTTP DELETE request, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT.
#' @param path Endpoint path.
#' @param parameters Request parameters.
#' @param file.name Name of file to save response to, or NULL to not save to a fil.
#' @return Response object.
#' @noRd
http.delete <- function(labbcat.url, path) {
    
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    resp <- httr::DELETE(url,
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
        return(http.delete(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

#' make an HTTP POST request, asking for credentials if required
#' @param labbcat.url URL to LaBB-CAT.
#' @param path Endpoint path.
#' @param parameters Request parameters.
#' @param file.name Name of file to save response to, or NULL to not save to a fil.
#' @return Response object.
#' @noRd
http.post.multipart <- function(labbcat.url, path, parameters, file.name=NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    ## print(url)
    
    ## attempt the request
    if (is.null(file.name)) {
        resp <- httr::POST(url,
                           ## httr::verbose(),
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
    ## print(paste("response ", resp))
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

#' Convert HTML to plain text for display purposes
#' @param html HTML content to convert.
#' @return Content with HTML tags stripped out.
#' @noRd
html.to.text <- function(html) {
    ## remove DOCTYPE declaration
    text <- stringr::str_replace(html, "<!DOCTYPE html>", "")
    ## remove title
    text <- gsub("<title>.*</title>", "", text)
    ## leading hyphen and trailing colon for defined terms
    text <- stringr::str_replace_all(text, "<dt>", "<dt>- ")
    text <- stringr::str_replace_all(text, "</dt>", ":</dt>")
    ## leading hyphen for list items
    text <- stringr::str_replace_all(text, "<li>", "<li>- ")
    ## leading # for h1
    text <- stringr::str_replace_all(text, "<h1>", "<h1># ")
    ## leading ## for h2
    text <- stringr::str_replace_all(text, "<h2>", "<h2>## ")
    ## leading ### for h3
    text <- stringr::str_replace_all(text, "<h3>", "<h3>### ")
    ## remove tags, retaining their contents
    pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
    text <- stringr::str_replace_all(text, pattern, "")
    return(text)
}

#' Determing file name of an HTTP response from the content-disposition header
#' @param content.disposition Value of the the content-disposition response header
#' @return The suggested file name of the download, or NULL if it could not be determined
#' @noRd
fileNameFromContentDisposition <- function(content.disposition) {
    if (!is.null(content.disposition) && content.disposition != "") {
        ## header is something like:
        ## attachment; filename=orthography_(a).csv; filename*="orthography%E2%89%88_%28%C3%A1%29.csv"
        content.disposition.filename <- strsplit(content.disposition, "filename\\*?=")
        if (length(content.disposition.filename[[1]]) > 1) {
            ## strip everythin after ; so we don't get the "; filename* part
            content.disposition.filename <- strsplit(content.disposition.filename[[1]][2], ";")
            filename <- content.disposition.filename[[1]][1]
            ## strip quotes around name if any
            filename = sub('"(.*)"', "\\1", filename)
            return(filename)
        } ## filename=...
    } ## there is a header value
    return(NULL)
}

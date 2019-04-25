#' \packageTitle{nzilbb.labbcat}
#'
#' \packageDescription{nzilbb.labbcat}
#'
#' \packageDESCRIPTION{nzilbb.labbcat}
#' \packageIndices{nzilbb.labbcat}
#' 
#' 'LaBB-CAT' is a web-based language corpus management system and this
#' package provides access to data stored in a 'LaBB-CAT' instance.
#' You must have at least version 20190424.1154 of 'LaBB-CAT' to use
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
#' ## Load some search results previously exported from LaBB-CAT
#' results <- read.csv("results.csv", header=T)
#' 
#' ## Get the phonemic transcriptions for the matches
#' phonemes <- getAnnotationLabels(labbcat.url, results$MatchId, "phonemes")
#'
#' ## Get sound fragments for the matches
#' wav.files <- getSoundFragment(labbcat.url, results$Transcript, results$Line, results$LineEnd)
#' }
#' 
NULL

### Internal variables:

## minimum version of LaBB-CAT required:
.min.labbcat.version <- "20190412.1154"

## HTTP request timeout
.request.timeout <- 10

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
    return(stringr::str_replace_all(URLencode(value),"\\+","%2B"))
}

## build a store call URL 
buildUrl <- function(labbcat.url, call, parameters = NULL) {
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")
    url <- paste(labbcat.url, "store?call=", call, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    return(url)
}

## make an HTTP GET request to the store URL, asking for credentials if required
store.get <- function(labbcat.url, call, parameters = NULL) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, "store?call=", call, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    
    ## attempt the request
    resp <- httr::GET(url, httr::timeout(.request.timeout))
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
            instance.ok <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means success, but wrong LaBB-CAT version
            if (is.null(instance.ok)) return(NULL)
            ## TRUE means everything OK
            if (instance.ok) break
        } ## next try
        
        ## and try again
        return(store.get(labbcat.url, call, parameters))
    } else {
        return(resp)
    }
}
## make an HTTP GET request, asking for credentials if required
http.get <- function(labbcat.url, path, parameters = NULL, content.type = "application/json") {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    
    ## attempt the request
    resp <- httr::GET(url, httr::timeout(.request.timeout), httr::add_headers("Accepts" = content.type))
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
            instance.ok <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means success, but wrong LaBB-CAT version
            if (is.null(instance.ok)) return(NULL)
            ## TRUE means everything OK
            if (instance.ok) break
        } ## next try
        
        ## and try again
        return(http.get(labbcat.url, path, parameters, content.type))
    } else {
        return(resp)
    }
}

## make an HTTP POST request, asking for credentials if required
http.post <- function(labbcat.url, path, parameters, file.name) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    
    ## attempt the request
    resp <- httr::POST(url,
                       httr::write_disk(file.name, overwrite=TRUE),
                       httr::timeout(.request.timeout),
                       body = parameters, encode = "form")
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
            instance.ok <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means success, but wrong LaBB-CAT version
            if (is.null(instance.ok)) return(NULL)
            ## TRUE means everything OK
            if (instance.ok) break
        } ## next try
        
        ## and try again
        return(http.post(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

## make an HTTP POST request, asking for credentials if required
http.post.multipart <- function(labbcat.url, path, parameters, file.name) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

    ## build request URL
    url <- paste(labbcat.url, path, sep="")
    
    ## attempt the request
    resp <- httr::POST(url,
                       httr::write_disk(file.name, overwrite=TRUE),
                       httr::timeout(.request.timeout),
                       body = parameters, encode = "multipart")
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
            instance.ok <- labbcatCredentials(
                labbcat.url,
                get.hidden.input(paste(instance.name, "Username:", "")),
                get.hidden.input(paste(instance.name, "Password:", "")))
            ## NULL means success, but wrong LaBB-CAT version
            if (is.null(instance.ok)) return(NULL)
            ## TRUE means everything OK
            if (instance.ok) break
        } ## next try
        
        ## and try again
        return(http.post.multipart(labbcat.url, path, parameters, file.name))
    } else {
        return(resp)
    }
}

## Export functions:

#' Sets the username and password that the package should use for connecting
#' to a given LaBB-CAT server in future function calls.
#'
#' This step is optional, as all functions will prompt the user for the username
#' and password if required.  If the script is running in RStudio, then the
#' RStudio password input dialog is used, hiding the credentials from view.
#' Otherwise, the console is used, and credentials are visible.
#'
#' The recommended approach is to *not* use labbcatCredentials, to avoid saving
#' user credentials in script files that may eventually become visible to other.
#' Use labbcatCredentials *only* in cases where the script execution is unsupervised.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param username The LaBB-CAT username, if it is password-protected
#' @param password The LaBB-CAT password, if it is password-protected
#' @return FALSE if the username/password are incorrect,
#' NULL if they username/password are correct but the version of LaBB-CAT
#' is incompatible with the package, and TRUE otherwise.
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## specify the username/password in the script
#' ## (only use labbcatCredentials for scripts that must execute unsupervised!)
#' labbcatCredentials(labbcat.url, "demo", "demo")
#' }
#'
#' @keywords connect username password timeout
#' 
labbcatCredentials <- function(labbcat.url, username, password) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")
    
    version.check.url <- paste(labbcat.url, "store?call=", sep="")
    authorization <- httr::authenticate(username, password)
    resp <- httr::GET(version.check.url, authorization, httr::timeout(.request.timeout))

    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 401) {
            return(FALSE)
        } else {
            print(paste("ERROR: ", httr::http_status(resp)$message))
            return(FALSE)
        }
    } ## not 200 OK

    ## do a second request
    ## - this seems to be required for credentials to 'take' in non-interactive mode
    resp <- httr::GET(version.check.url, authorization, httr::timeout(.request.timeout))
    
    ## check the LaBB-CAT version
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    resp.json <- jsonlite::fromJSON(resp.content)
    version <- resp.json$model$version
    if (is.null(version) || version < .min.labbcat.version) {
        print(paste("ERROR:", labbcat.url, "is version", version, "but the minimum version is", .min.labbcat.version))
        return(NULL)
    }        
    return(TRUE)    
}

#' Gets the store's ID.
#' 
#' The store's ID - i.e. the ID of the 'LaBB-CAT' instance.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return The annotation store's ID
#' @examples
#' \dontrun{
#' ## Get ID of LaBB-CAT instance
#' instance.id <- getId("https://labbcat.canterbury.ac.nz/demo/")
#' }
#'
getId <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getId")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of layer IDs.
#'
#' Layer IDs are annotation 'types'.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of layer IDs
#' 
#' @examples
#' \dontrun{
#' ## Get names of all layers
#' layer.ids <- getLayerIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords layer
#' 
getLayerIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getLayerIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of layer definitions.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of layer definitions, with members:
#' \enumerate{
#'  \item{id The layer's unique ID}
#'  \item{parentId The layer's parent layer ID}
#'  \item{description The description of the layer}
#'  \item{alignment The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment}
#'  \item{peers Whether children have peers or not}
#'  \item{peersOverlap Whether child peers can overlap or not}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{saturated Whether children must temporally fill the entire parent duration (true) or not (false)}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{type The type for labels on this layer}
#'  \item{validLabels List of valid label values for this layer}
#' }
#' 
#' @seealso \code{\link{getLayerIds}}
#' @examples
#' \dontrun{
#' ## Get definitions of all layers
#' layers <- getLayers("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords layer
#' 
getLayers <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getLayers")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a layer definition.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id ID of the layer to get the definition for
#' @return The definition of the given layer, with members:
#' \enumerate{
#'  \item{id The layer's unique ID}
#'  \item{parentId The layer's parent layer ID}
#'  \item{description The description of the layer}
#'  \item{alignment The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment}
#'  \item{peers Whether children have peers or not}
#'  \item{peersOverlap Whether child peers can overlap or not}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{saturated Whether children must temporally fill the entire parent duration (true) or not (false)}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{type The type for labels on this layer}
#'  \item{validLabels List of valid label values for this layer}
#' }
#' 
#' @seealso \code{\link{getLayerIds}}
#' \code{\link{getLayers}}
#' @examples
#' \dontrun{
#' ## Get the definition of the orthography layer
#' orthography.layer <- getLayer("https://labbcat.canterbury.ac.nz/demo/", "orthography")
#' }
#'
#' @keywords layer
#' 
getLayer <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getLayer", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#' 
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of corpus IDs
#' 
#' @examples
#' \dontrun{
#' ## List corpora
#' corpora <- getCorpusIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords corpora
#' 
getCorpusIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getCorpusIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' List the predefined media tracks available for transcripts.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of media track definitions.
#' @examples 
#' \dontrun{
#' ## Get the media tracks configured in LaBB-CAT
#' tracks <- getMediaTracks("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords media sound
#' 
getMediaTracks <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getMediaTracks")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of participant IDs.
#'
#' Returns a list of participant IDs.
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of participant IDs
#' 
#' @examples
#' \dontrun{
#' ## List all speakers
#' speakers <- getParticipantIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords speaker participant
#' 
getParticipantIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getParticipantIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of graph IDs.
#'
#' Returns a list of graph IDs (i.e. transcript names).
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of graph IDs
#' 
#' @examples 
#' \dontrun{
#' ## List all transcripts
#' transcripts <- getGraphIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords graph transcript
#' 
getGraphIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getGraphIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#'
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID (name) of the corpus
#' @return A list of corpus IDs
#' 
#' @examples 
#' \dontrun{
#' ## List transcripts in the QB corpus
#' transcripts <- getGraphIdsInCorpus("https://labbcat.canterbury.ac.nz/demo/", "QB")
#' }
#' 
#' @keywords corpora corpus
#' 
getGraphIdsInCorpus <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getGraphIdsInCorpus", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of IDs of graphs that include the given participant.
#'
#' Returns a list of IDs of graphs (i.e. transcript names) that include
#' the given participant.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A participant ID
#' @return A list of graph IDs
#' 
#' @seealso \code{\link{getParticipantIds}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
#' transcripts <- getGraphIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
#' }
#' 
#' @keywords graph transcript
#' 
getGraphIdsWithParticipant <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getGraphIdsWithParticipant", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a list of IDs of graphs that match a particular pattern.
#'
#' Gets a list of IDs of graphs (i.e. transcript names) that match a
#' particular pattern.
#'
#' The results can be exhaustive, by omitting pageLength and
#' pageNumber, or they  can be a subset (a 'page') of results, by
#' given pageLength and pageNumber values.
#'
#' The order of the list can be specified.  If ommitted, the graphs
#' are listed in ID order.
#'
#' The expression language is currently not well defined, but
#' expressions such as those in the examples can be used.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which graphs match
#' @param pageLength The maximum number of IDs to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @param order An expression that determines the order the graphs are
#' listed in
#' @return A list of graph IDs (i.e. transcript names)
#' 
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all transcripts whose names start with "BR"
#' transcripts <- getMatchingGraphIds(labbcat.url, "id MATCHES 'BR.+'")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "my('corpus').label = 'QB'", 20, 0)
#' 
#' ## Get the second transcript that has "QB247_Jacqui" as a speaker
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "'QB247_Jacqui' IN labels('who')", 1, 1)
#' 
#' ## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
#' ## in word-count order 
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('who')", 1, 1,
#'         "my('transcript_word_count').label")
#' }
#' 
#' @keywords graph transcript expression
#' 
getMatchingGraphIds <- function(labbcat.url, expression, pageLength = NULL, pageNumber = NULL, order = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    if (!is.null(order)) parameters <- append(parameters, list(order=order))
    resp <- store.get(labbcat.url, "getMatchingGraphIdsPage", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets the number of annotations on the given layer of the given graph.
#'
#' Returns the number of annotations on the given layer of the given
#' graph (transcript).
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @return The number of annotations on that layer
#' 
#' @seealso
#' \code{\link{getGraphIds}}
#' \code{\link{getGraphIdsInCorpus}}
#' \code{\link{getGraphIdsWithParticipant}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Count the number of words in UC427_ViktoriaPapp_A_ENG.eaf
#' token.count <- countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' }
#' 
#' @keywords graph transcript
#' 
countAnnotations <- function(labbcat.url, id, layerId) {
    parameters <- list(id=id, layerId=layerId)
    resp <- store.get(labbcat.url, "countAnnotations", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets the annotations on the given layer of the given graph.
#'
#' Returns the annotations on the given layer of the given graph
#' (transcript).
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @param pageLength The maximum number of annotations to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @return A named list of annotations, with members:
#' \enumerate{
#'  \item{id The annotation's unique ID}
#'  \item{layerId The name of the layer it comes from}
#'  \item{label The value of the annotation}
#'  \item{startId The ID of the start anchor},
#'  \item{endId The ID of the end anchor},
#'  \item{parentId The ID of the parent annotation},
#'  \item{ordinal The ordinal of the annotation among its peers},
#'  \item{confidence A rating from 0-100 of the confidence of the label
#'  e.g. 10: default value, 50: automatically generated, 100: manually annotated}
#' }
#' 
#' @seealso 
#'   \code{\link{getGraphIds}}
#'   \code{\link{getGraphIdsInCorpus}}
#'   \code{\link{getGraphIdsWithParticipant}}
#'   \code{\link{countAnnotations}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' }
#'
#' @keywords graph transcript
#' 
getAnnotations <- function(labbcat.url, id, layerId, pageLength = NULL, pageNumber = NULL) {
    parameters <- list(id=id, layerId=layerId)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    resp <- store.get(labbcat.url, "getAnnotations", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets the given anchors in the given graph.
#'
#' Lists the given anchors in the given graph (transcript).
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param anchorId A vector of anchor IDs (or a string representing one anchor ID)
#' @return  A named list of anchors, with members:
#' \enumerate{
#'  \item{id The annotation's unique ID},
#'  \item{offset The offset from the beginning (in seconds if it's a
#'  transcript of a recording, or in characters if it's a text document)}
#'  \item{confidence A rating from 0-100 of the confidence of the offset,
#'   e.g. 10: default value, 50: force-aligned, 100: manually aligned}
#' }
#' 
#' @seealso \link{getAnnotations}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' 
#' ## Get the start anchors for the above tokens
#' word.starts <- getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
#' }
#' 
#' @keywords anchor
#' 
getAnchors <- function(labbcat.url, id, anchorId) {
    parameters <- list(id=id)
    for (id in anchorId) parameters <- append(parameters, list(anchorId=id))
    resp <- store.get(labbcat.url, "getAnchors", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' List the media available for the given graph.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @return A named list of media files available for the given graph, with members:
#' \enumerate{
#'  \item{trackSuffix The track suffix of the media}
#'  \item{mimeType The MIME type of the file}
#'  \item{url URL to the content of the file}
#'  \item{name Name of the file}
#' }
#' 
#' @seealso \link{getGraphIds}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## List the media files available for BR2044_OllyOhlson.eaf
#' media <- getAvailableMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
#' }
#' 
#' @keywords media audio
#' 
getAvailableMedia <- function(labbcat.url, id) {
    parameters <- list(id=id)
    resp <- store.get(labbcat.url, "getAvailableMedia", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a given media track for a given graph.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param trackSuffix The track suffix of the media
#' @param mimeType The MIME type of the media
#' @return A URL to the given media for the given graph
#' @seealso \link{getGraphIds}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get URL for the WAV file for BR2044_OllyOhlson.eaf
#' media <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
#' 
#' ## Get URL for the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
#' media <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")
#' }
#' 
#' @keywords media audio
#' 
getMedia <- function(labbcat.url, id, trackSuffix = "", mimeType = "audio/wav") {
    parameters <- list(id=id, trackSuffix=trackSuffix, mimeType=mimeType)
    resp <- store.get(labbcat.url, "getMedia", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}

#' Gets a sound fragment from 'LaBB-CAT'.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The graph ID (transcript name) of the sound recording, or
#'     a vector of graph IDs. 
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param sampleRate Optional sample rate in Hz - if a positive
#'     integer, then the result is a mono file with the given sample rate.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return The name of the file, which is saved in the current
#'     directory, or a list of names of files, if multiple
#'     id's/start's/end's were specified 
#'
#' If a list of files is returned, they are in the order that they
#'     were returned by the server, which *should* be the order that
#'     they were specified in the id/start/end lists.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get the 5 seconds starting from 10s after the beginning of a recording
#' wav.file <- getSoundFragment(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0)
#' 
#' ## Get the 5 seconds starting from 10s as a mono 22kHz file
#' wav.file <- getSoundFragment(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
#' 
#' ## Load some search results previously exported from LaBB-CAT
#' results <- read.csv("results.csv", header=T)
#' 
#' ## Get a list of fragments
#' wav.files <- getSoundFragment(labbcat.url, results$Transcript, results$Line, results$LineEnd)
#' 
#' ## Get a list of fragments with no prgress bar
#' wav.file <- getSoundFragment(
#'               labbcat.url, results$Transcript, results$Line, results$LineEnd, no.progress=TRUE)
#' }
#' @keywords sample sound fragment wav
#' 
getSoundFragment <- function(labbcat.url, id, start, end, sampleRate = NULL, no.progress=FALSE) {
    if (length(id) == 1) { ## one fragment
        dir <- ""
    } else { ## multiple fragments
        ## save fragments into their own directory
        dir <- "fragments"
        if (file.exists(dir)) {
            ## ensure it's a new directory by adding a number
            n <- 1
            new.dir = paste(dir,"(",n,")", sep="")
            while (file.exists(new.dir)) {
                n <- n + 1
                new.dir = paste(dir,"(",n,")", sep="")
            } # next try
            dir <- new.dir
        }
        dir.create(dir)
        ## add trailing slash
        dir <- paste(dir, .Platform$file.sep, sep="")
    }

    pb <- NULL
    if (!no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## loop throug each triple, getting fragments individually
    ## (we could actually pass the lot to LaBB-CAT in one go and get a ZIP file back
    ##  but then we can't be sure the results contain a row for every fragment specified
    ##  and we can't display a progress bar)
    file.names = c()
    r <- 1
    for (graph.id in id) {
        parameters <- list(id=graph.id, start=start[r], end=end[r])
        if (!is.null(sampleRate)) parameters <- list(id=graph.id, start=start[r], end=end[r], sampleRate=sampleRate)
        file.name <- paste(dir, stringr::str_replace(graph.id, "\\.[^.]+$",""), "__", start[r], "-", end[r], ".wav", sep="")

        tryCatch({
            resp <- http.post(labbcat.url, "soundfragment", parameters, file.name)
            if (httr::status_code(resp) != 200) { # 200 = OK
                print(paste("ERROR: ", httr::http_status(resp)$message))
                if (httr::status_code(resp) != 404) { # 404 means the audio wasn't on the server
                    ## some other error occurred so print what we got from the server
                    print(readLines(file.name))
                }
                file.remove(file.name)
                file.name <<- NULL
            } else {
                content.disposition <- as.character(httr::headers(resp)["content-disposition"])
                content.disposition.parts <- strsplit(content.disposition, "=")
                if (length(content.disposition.parts[[1]]) > 1
                    && file.name != content.disposition.parts[[1]][2]) {
                    ## file name is specified, so use it
                    final.file.name <- paste(dir, content.disposition.parts[[1]][2], sep="")
                    file.rename(file.name, final.file.name)
                    file.name <- final.file.name
                }
            }
        }, error = function(e) {
            print(paste("ERROR:", e))
            file.name <<- NULL
        })
        file.names <- append(file.names, file.name)
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    if (!is.null(pb)) close(pb)
    return(file.names)   
}

#' Gets labels of annotations on a given layer, identified by given annotation IDs.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A vector of annotation IDs. 
#' @param layerId A layer name.
#' @param count The number of annotations on the given layer to retrieve.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return A data frame of labels.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Load some search results previously exported from LaBB-CAT
#' results <- read.csv("results.csv", header=T)
#'
#' ## Get the topic annotations for the matches
#' topics <- getAnnotationLabels(labbcat.url, results$MatchId, "topic")
#' }
#' 
#' @keywords layer annotation label
#' 
getAnnotationLabels <- function(labbcat.url, id, layerId, count=1, no.progress=FALSE) {

    pb <- NULL
    if (!no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## we need a vector of size 'count' to store vectors of labels
    labels = c()
    for (col in 1:count) labels <- append(labels, c())
    
    ## loop through each id, getting fragments individually
    r <- 1
    for (annotation.id in id) {
        ## if the ID is actually a URL or MatchId, pick out the ew_0_n+ part
        annotation.id <- stringr::str_match(annotation.id, "ew_0_[0-9]+")
        ## TODO can't necessarily assume that annotation.id is on the transcript layer
        expression = paste("my('transcript').id = '", annotation.id, "' AND layer.id = '",layerId,"'", sep="")
        parameters <- list(expression=expression, pageLength=count, pageNumber=0)
        resp <- store.get(labbcat.url, "getMatchingAnnotations", parameters)
        if (is.null(resp)) return()
        
        ## create default row
        row <- c()
        ## fill it with NA
        for (col in 1:count) row <- append(row, NA)

        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
        } else {
            resp.json <- jsonlite::fromJSON(resp.content)
            for (error in resp.json$errors) print(error)
            if (length(resp.json$model$result) > 0) {
                ## populate the row
                for (col in 1:count) {
                    row[col] <- resp.json$model$result$label[col]
                }
            }
        }
        labels <- append(labels, row)
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    if (!is.null(pb)) close(pb)
    cols <- c()
    for (col in 1:count) {
        if (count == 1) {
            col.name <- layerId
        } else {
            col.name <- paste(layerId,".",col,sep="")
        }
        cols <- append(cols, col.name)
    }
    labels.matrix <- matrix(labels, ncol=count, byrow=TRUE)
    colnames(labels.matrix) <- cols
    labels.df <- as.data.frame(labels.matrix, col.names=cols)

    return(labels.df)
}

#' List the dictionaries available.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A named list of layer manager IDs, each of which containing a list of
#' dictionaries that the layer manager makes available.
#' 
#' @seealso \link{getDictionaryEntries}
#' @examples 
#' \dontrun{
#' ## List the dictionaries available
#' dictionaries <- getDictionaries("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords dictionary
#' 
getDictionaries <- function(labbcat.url) {
    resp <- http.get(labbcat.url, "dictionaries")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model)
}

#' Lookup entries in a dictionary.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param managerId The layer manager ID of the dictionary, as returned by getDictionaries
#' @param dictionaryId The ID of the dictionary, as returned by getDictionaries
#' @param keys A list of entries to look up
#' @return A data frame with the keys and their dictionary entries.
#' 
#' @seealso \link{getDictionaries}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#'
#' keys <- c("the", "quick", "brown", "fox")
#' 
#' ## get the pronunciations according to CELEX
#' entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)", keys)
#' }
#' 
#' @keywords dictionary
#' 
getDictionaryEntries <- function(labbcat.url, managerId, dictionaryId, keys) {
    ## save keys to a CSV file
    upload.file = "keys.csv"
    download.file = "entries.csv"
    write.table(keys, upload.file, sep=",", row.names=FALSE, col.names=FALSE)

    ## make request
    parameters <- list(managerId=managerId, dictionaryId=dictionaryId, uploadfile=httr::upload_file(upload.file))    
    resp <- http.post.multipart(labbcat.url, "dictionary", parameters, download.file)

    ## tidily remove upload file
    file.remove(upload.file)

    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }

    ## load the returned entries
    entries <- read.csv(download.file, header=F)

    ## rename the columns so that the one containing the keys is called "key"
    colnames(entries) <- c("key", head(colnames(entries), length(colnames(entries)) - 1))

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(entries)
}

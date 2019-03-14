#' \packageTitle{nzilbb.labbcat}
#'
#' \packageDescription{nzilbb.labbcat}
#'
#' \packageDESCRIPTION{nzilbb.labbcat}
#' \packageIndices{nzilbb.labbcat}
#' 
#' LaBB-CAT is a web-based language corpus management system and this
#' package provides access to data stored in a LaBB-CAT instance.
#' 
#' As LaBB-CAT instances are usually password-protected, the function
#' \code{labbcat.instance} must be used first of all to create an object
#' that provides access to a specific instance of LaBB-CAT with specific
#' credentials.  This object is then used as the first argument for all
#' other function calls.
#' 
#' @docType package
#' @keywords package
#' @name nzilbb.labbcat
#' @author \packageAuthor{nzilbb.labbcat}
#' @references
#' \cite{Robert Fromont and Jennifer Hay, "{ONZE Miner}: the development of a browser-based research tool", 2008}
#' \cite{Robert Fromont, "Toward a format-neutral annotation store", 2017}
#' @examples
#' ## connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the 5 seconds starting from 10s as a mono 22kHz file
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
#' 
NULL

### Internal variables:

## minimum version of LaBB-CAT required:
.min.labbcat.version <- "20190312.1838"

## encode a parameter value for inclusion in the URL
enc <- function(value) {
    return(stringr::str_replace_all(URLencode(value),"\\+","%2B"))
}

## build a store call URL 
buildUrl <- function(labbcat, call, parameters = NULL) {
    url <- paste(labbcat$storeUrl, call, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    return(url)
}

## Export functions:

#' Connects to the given LaBB-CAT instance, and returns and object that
#' must be used for all other functions.
#'
#' If a username and password are not passed, and the LaBB-CAT instance
#' is password-protected (and the function is called in interactive
#' mode), then the user will be prompted for the username and
#' password. This is the recommended method for accessing
#' password-protected LaBB-CAT instances, in order to avoid saving
#' passwords in script files. The username and password parameters are
#' provided for cases where the script is not run in interactive mode.
#' 
#' @param url URL to the LaBB-CAT instance
#' @param username The LaBB-CAT username, if it is password-protected
#' @param password The LaBB-CAT password, if it is password-protected
#' @param timeout Maximum time for any LaBB-CAT request
#' @return An object that can be passed as the labbcat parameter for
#'     other functions in this package 
#' @examples
#' ## connect to an open or password-protected instance of LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/")
#'
#' ## connect to a password-protected instance of LaBB-CAT with explicit credentials
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/",
#'      username="demo", password="demo")
#'
#' @keywords connect username password timeout
#' 
labbcat.instance <- function(url, username = NULL, password = NULL, timeout = 10) {
    baseUrl <- url
    ## ensure baseUrl has a trailing slash
    if (!grepl("/$", url)) baseUrl <- paste(url, "/", sep="")
    
    storeUrl <- paste(baseUrl, "store?call=", sep="")
    
    if (is.null(username)) {
        authorization <- NULL
        resp <- httr::GET(storeUrl, httr::timeout(timeout))
    } else {  
        authorization <- httr::authenticate(username, password)
        resp <- httr::GET(storeUrl, authorization, httr::timeout(timeout))
    }
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 401 && is.null(username) && is.null(password)) {
            ## it's password-protected, but they haven't provided credentials
            ## so ask them for the username and password
            return(labbcat.instance(
                url, readline("LaBB-CAT Username: "), readline("LaBB-CAT Password: ")))
        } else {
            print(paste("ERROR: ", httr::http_status(resp)$message))
            return(NULL)
        }
    } else { ## respons was OK
        ## check the LaBB-CAT version
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        resp.json <- jsonlite::fromJSON(resp.content)
        version <- resp.json$model$version
        if (is.null(version) || version < .min.labbcat.version) {
            print(paste("ERROR:", baseUrl, "is version", version, "but the minimum version is", .min.labbcat.version))
            return(NULL)
        } else { ## everything OK
            return (list(
                baseUrl = baseUrl,
                version = version,
                storeUrl = storeUrl,
                timeout = timeout,
                authorization = authorization
            ))
        }
    }
}

#' Gets the store's ID.
#' 
#' The store's ID - i.e. the ID of the LaBB-CAT instance.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return The annotation store's ID
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get ID of LaBB-CAT instance
#' instance.id <- labbcat.getId(labbcat)
#'
labbcat.getId <- function(labbcat) {
    url <- buildUrl(labbcat, "getId")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of layer IDs.
#'
#' Layer IDs are annotation 'types'.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of layer IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get names of all layers
#' layer.ids <- labbcat.getLayerIds(labbcat)
#' 
#' @keywords layer
#' 
labbcat.getLayerIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getLayerIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a layer definition.
#' 
#' Gets a layer definition.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id ID of the layer to get the definition for
#' @return The definition of the given layer
#' 
#' @seealso \code{\link{labbcat.getLayerIds}}
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the configuration of the orthography layer
#' orthography.layer <- labbcat.getLayer(labbcat, "orthography")
#'
#' @keywords layer
#' 
labbcat.getLayer <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getLayer", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#' 
#' Returns a list of corpora in the given LaBB-CAT instance.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of corpus IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List corpora
#' corpora <- labbcat.getCorpusIds(labbcat)
#' 
#' @keywords corpora
#' 
labbcat.getCorpusIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getCorpusIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of participant IDs.
#'
#' Returns a list of participant IDs.
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of participant IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List all speakers
#' speakers <- labbcat.getParticipantIds(labbcat)
#' 
#' @keywords speaker participant
#' 
labbcat.getParticipantIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getParticipantIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of graph IDs.
#'
#' Returns a list of graph IDs (i.e. transcript names).
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of graph IDs
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List all transcripts
#' transcripts <- labbcat.getGraphIds(labbcat)
#' 
#' @keywords graph transcript
#' 
labbcat.getGraphIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getGraphIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#'
#' Returns a list of corpora in the given LaBB-CAT instance.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id The ID (name) of the corpus
#' @return A list of corpus IDs
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List corpora
#' corpora <- labbcat.getCorpusIds(labbcat)
#' 
#' @keywords corpora corpus
#' 
labbcat.getGraphIdsInCorpus <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getGraphIdsInCorpus", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of IDs of graphs that include the given participant.
#'
#' Returns a list of IDs of graphs (i.e. transcript names) that include
#' the given participant.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A participant ID
#' @return A list of graph IDs
#' 
#' @seealso \code{\link{labbcat.getParticipantIds}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
#' transcripts <- labbcat.getGraphIdsWithParticipant(labbcat, "UC427_ViktoriaPapp_A_ENG")
#' 
#' @keywords graph transcript
#' 
labbcat.getGraphIdsWithParticipant <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getGraphIdsWithParticipant", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
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
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param expression An expression that determines which graphs match
#' @param pageLength The maximum number of IDs to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @param order An expression that determines the order the graphs are
#' listed in
#' @return A list of graph IDs (i.e. transcript names)
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get all transcripts whose names start with "BR"
#' transcripts <- labbcat.getMatchingGraphIdsPage(labbcat, "id MATCHES 'BR.+'")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "my('corpus').label = 'QB'", 20, 0)
#' 
#' ## Get the second transcript that has "QB247_Jacqui" as a speaker
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "'QB247_Jacqui' IN labels('who')", 1, 1)
#' 
#' ## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
#' ## in word-count order 
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('who')", 1, 1,
#'         "my('transcript_word_count').label")
#' 
#' @keywords graph transcript expression
#' 
labbcat.getMatchingGraphIdsPage <- function(labbcat, expression, pageLength = NULL, pageNumber = NULL, order = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    if (!is.null(order)) parameters <- append(parameters, list(order=order))
    url <- buildUrl(labbcat, "getMatchingGraphIdsPage", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the number of annotations on the given layer of the given graph.
#'
#' Returns the number of annotations on the given layer of the given
#' graph (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @return The number of annotations on that layer
#' 
#' @seealso
#' \code{\link{labbcat.getGraphIds}}
#' \code{\link{labbcat.getGraphIdsInCorpus}}
#' \code{\link{labbcat.getGraphIdsWithParticipant}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Count the number of words in UC427_ViktoriaPapp_A_ENG.eaf
#' token.count <- labbcat.countAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' @keywords graph transcript
#' 
labbcat.countAnnotations <- function(labbcat, id, layerId) {
    parameters <- list(id=id, layerId=layerId)
    url <- buildUrl(labbcat, "countAnnotations", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the annotations on the given layer of the given graph.
#'
#' Returns the annotations on the given layer of the given graph
#' (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
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
#'   \code{\link{labbcat.getGraphIds}}
#'   \code{\link{labbcat.getGraphIdsInCorpus}}
#'   \code{\link{labbcat.getGraphIdsWithParticipant}}
#'   \code{\link{labbcat.countAnnotations}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#'
#' @keywords graph transcript
#' 
labbcat.getAnnotations <- function(labbcat, id, layerId, pageLength = NULL, pageNumber = NULL) {
    parameters <- list(id=id, layerId=layerId)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    url <- buildUrl(labbcat, "getAnnotations", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the given anchors in the given graph.
#'
#' Lists the given anchors in the given graph (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
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
#' @seealso \link{labbcat.getAnnotations}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' 
#' ## Get the start anchors the above tokens
#' word.starts <- labbcat.getAnchors(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
#' 
#' @keywords anchor
#' 
labbcat.getAnchors <- function(labbcat, id, anchorId) {
    parameters <- list(id=id)
    for (id in anchorId) parameters <- append(parameters, list(anchorId=id))
    url <- buildUrl(labbcat, "getAnchors", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a sound fragment from LaBB-CAT.
#'
#' Extracts part of a WAV file.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id The graph ID (transcript name) of the sound recording
#' @param start The start time in seconds
#' @param end The end time in seconds
#' @param sampleRate Optional sample rate in Hz - if a positive integer, then the
#'    result is a mono file with the given sample rate 
#' @return The name of the file, which is saved in the current directory
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the 5 seconds starting from 10s after the beginning of a recording
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0)
#' 
#' ## Get the 5 seconds starting from 10s as a mono 22kHz file
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
#' 
#' @keywords sample sound fragment wav
#' 
labbcat.getSoundFragment <- function(labbcat, id, start, end, sampleRate = NULL) {
    url <- paste(labbcat$baseUrl, "soundfragment", sep="")
    parameters <- list(id=id, start=start, end=end)
    if (!is.null(sampleRate)) parameters <- list(id=id, start=start, end=end, sampleRate=sampleRate)
    filename <- paste(stringr::str_replace(id, "\\.[^.]+$",""), "__", start, "-", end, ".wav", sep="")
    tryCatch({
        resp <- httr::POST(url, labbcat$authorization, httr::write_disk(filename, overwrite=TRUE), httr::timeout(labbcat$timeout), body = parameters, encode = "form")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            if (httr::status_code(resp) != 404) { # 404 means the audio wasn't on the server
                ## some other error occurred so print what we got from the server
                print(readLines(filename))
            }
            file.remove(filename)
            return(NULL)
        }
        content.disposition <- as.character(httr::headers(resp)["content-disposition"])
        content.disposition.parts <- strsplit(content.disposition, "=")
        if (length(content.disposition.parts[[1]]) > 1
            && filename != content.disposition.parts[[1]][2]) {
            ## file name is specified, so use it
            file.rename(filename, content.disposition.parts[[1]][2])
            filename <- content.disposition.parts[[1]][2]
        }
    }, error = function(e) {
        print(paste("ERROR:", e))
        filename <<- NULL
    })
    return(filename)
}

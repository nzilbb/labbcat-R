#' Gets version information of all components of LaBB-CAT.
#' 
#' Version information includes versions of all components and modules installed on the
#' LaBB-CAT server, including format converters and annotator modules.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return The versions of different components of LaBB-CAT, divided into sections:
#'  - *System* Overall LaBB-CAT system components
#'  - *Formats* Annotation format conversion modules
#'  - *Layer Managers* Annotator module versions
#'  - *3rd Pary Software* Versions of software installed on the server that
#'        LaBB-CAT integrates with, e.g. Praat, FastTrack, etc.
#'  - *RDBMS* MySQL Server version information
#' @examples
#' \dontrun{
#' ## Get ID of LaBB-CAT instance
#' versionInfo <- labbcatVersionInfo("https://labbcat.canterbury.ac.nz/demo/")
#' print(paste("LaBB-CAT version", versionInfo$System$`LaBB-CAT`, " Full version info:"))
#' print(t(as.data.frame(versionInfo)))
#' }
#'
labbcatVersionInfo <- function(labbcat.url) {
    resp <- http.get(labbcat.url, "version")
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


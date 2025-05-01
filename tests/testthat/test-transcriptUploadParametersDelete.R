# NB using a local server for these tests, as 'edit' credentials are required
labbcat.url <- Sys.getenv('TEST_ADMIN_LABBCAT_URL')
username <- Sys.getenv('TEST_ADMIN_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_ADMIN_LABBCAT_PASSWORD')
test.transcript.id <- "labbcat-R.test.txt"

test_that("transcriptUpload and transcriptUploadParameters work", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## ensure it's not already there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 0)
    
    ## get attributes for new transcript
    corpus <- getCorpusIds(labbcat.url)[1]
    transcript.type <- getLayer(labbcat.url, "transcript_type")$validLabels[[1]]
    
    ## upload transcript
    response <- transcriptUpload(labbcat.url, test.transcript.id)
    id <- response$id
    parameters <- response$parameters

    ## set the parameters
    parameterValues <- list()
    ## first set default values
    for(p in 1:length(parameters$name)) parameterValues[parameters$name[p]] <- parameters$value[p]
    ## now set our own corpus/type
    parameterValues$labbcat_corpus <- corpus
    parameterValues$labbcat_transcript_type <- transcript.type
    transcript.id <- transcriptUploadParameters(labbcat.url, id, parameterValues)
    expect_equal(transcript.id, test.transcript.id)
    
    ## ensure it's been added
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 1)
    
    ## delete transcript
    deleteTranscript(labbcat.url, transcript.id)
})

test_that("transcriptUpload and transcriptUploadDelete work", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## ensure it's not already there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 0)
    
    ## upload transcript
    response <- transcriptUpload(labbcat.url, test.transcript.id)
    id <- response$id

    ## cancel the upload
    transcriptUploadDelete(labbcat.url, id)
    
    ## ensure it hasn't been added
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 0)    
})

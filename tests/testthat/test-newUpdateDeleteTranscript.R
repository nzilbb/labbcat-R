# NB using a local server for these tests, as 'edit' credentials are required
labbcat.url <- "http://localhost:8080/labbcat"
test.transcript.id <- "labbcat-R.test.txt"

test_that("newTranscript works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## ensure it's not already there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 0)
    
    ## get attributes for new transcript
    corpus <- getCorpusIds(labbcat.url)[1]
    transcript.type <- getLayer(labbcat.url, "transcript_type")$validLabels[[1]]
    
    ## upload transcript
    newTranscript(
        labbcat.url, test.transcript.id, transcript.type=transcript.type, corpus=corpus,
        episode="test")
    
    ## ensure it's been added
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 1)
})

test_that("updateTranscript works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## ensure it's already there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 1)
    
    ## upload transcript
    updateTranscript(labbcat.url, test.transcript.id)

    # ensure it's still there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 1)
})

test_that("deleteTranscript works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## ensure it's already there
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 1)
    
    ## upload transcript
    deleteTranscript(labbcat.url, test.transcript.id)

    # ensure it's gone
    ids <- getMatchingTranscriptIds(
        labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
    expect_equal(length(ids), 0)
})

labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getTranscriptAttributes works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    transcriptIds <- getMatchingTranscriptIds(labbcat.url, "id MATCHES 'BR.+'")
    attributes <- getTranscriptAttributes(
        labbcat.url,
        transcriptIds,
        c("transcript_language", "transcript_duration", "corpus"))

    ## check dataframe columns
    expect_equal(length(attributes$transcript), length(transcriptIds))
    expect_equal(length(attributes$transcript_language), length(transcriptIds))
    expect_equal(length(attributes$transcript_duration), length(transcriptIds))
    expect_equal(length(attributes$corpus), length(transcriptIds))

})


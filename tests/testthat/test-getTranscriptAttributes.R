labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getTranscriptAttributes works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    transcriptIds <- getMatchingTranscriptIds(labbcat.url, "id MATCHES 'BR.+'")
    attributes <- getTranscriptAttributes(
        labbcat.url,
        transcriptIds,
        c("transcript_language", "transcript_duration", "corpus"))

    ## check dataframe columns
    expect_equal(length(attributes$graph), length(transcriptIds))
    expect_equal(length(attributes$transcript_language), length(transcriptIds))
    expect_equal(length(attributes$transcript_duration), length(transcriptIds))
    expect_equal(length(attributes$corpus), length(transcriptIds))

})


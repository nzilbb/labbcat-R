labbcat.url <- Sys.getenv('TEST_ADMIN_LABBCAT_URL')
username <- Sys.getenv('TEST_ADMIN_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_ADMIN_LABBCAT_PASSWORD')

test_that("getFragmentAnnotations works with vectors", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## Get a list of span annotations
    topics <- getMatches(labbcat.url, list("topic" = list(pattern = '.*quake.*', target = TRUE)))
    
    ## Get the words and orthographies of the first view
    topics <- head(topics)
    tokens <- getFragmentAnnotations(
        labbcat.url, topics$Transcript, topics$Participant,
        topics$Target.topic.start, topics$Target.topic.end, c("word", "orthography")) 

    ## has the right columns
    expect_equal(length(tokens$word), 6)
    expect_equal(length(tokens$word.start), 6)
    expect_equal(length(tokens$word.end), 6)
    expect_equal(length(tokens$orthography), 6)
    expect_equal(length(tokens$orthography.start), 6)
    expect_equal(length(tokens$orthography.end), 6)
    ## doesn't have extra columns
    expect_equal(length(colnames(tokens)), 6)
})

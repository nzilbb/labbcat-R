labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("processWithPraat works with default format measures", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    
    ## search for KIT vowels
    pattern <- list(columns = list(list(layers = list(segment = list(pattern = "I")))))
    
    ## get the first 5 matches
    matches <- getMatches(labbcat.url, pattern, max.matches=5)
    
    ## process with praat
    formants <- processWithPraat(
        labbcat.url,
        matches$MatchId, matches$Target.segment.start, matches$Target.segment.end,
        praatScriptFormants(),
        0.025)

    expect_equal(length(formants$time_0_5), 5)
    expect_equal(length(formants$f1_time_0_5), 5)
    expect_equal(length(formants$f2_time_0_5), 5)
    expect_equal(length(formants$Error), 5)
})

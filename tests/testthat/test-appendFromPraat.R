labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("appendFromPraat works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    
    ## get the first 5 KIT vowels
    matches <- getMatches(labbcat.url, list(segment="I"), max.matches=5)
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## process with praat
    newMatches <- matches |> appendFromPraat(
                                 Target.segment.start, Target.segment.end,
                                 praatScriptFormants(),
                                 0.025)
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$time_0_5))
    expect_false(is.null(newMatches$f1_time_0_5))
    expect_false(is.null(newMatches$f2_time_0_5))
    expect_false(is.null(newMatches$Error))
})

test_that("appendFromPraat URL inference works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    
    ## get the first 5 KIT vowels
    matches <- getMatches(labbcat.url, list(segment="I"), max.matches=5)
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    attr(matches, "labbcat.url") <- NULL
    expect_true(is.null(attr(matches, "labbcat.url")))
    
    ## process with praat
    newMatches <- matches |> appendFromPraat(
                                 Target.segment.start, Target.segment.end,
                                 praatScriptFormants(),
                                 0.025)
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$time_0_5))
    expect_false(is.null(newMatches$f1_time_0_5))
    expect_false(is.null(newMatches$f2_time_0_5))
    expect_false(is.null(newMatches$Error))
})

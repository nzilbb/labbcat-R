labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("appendOffsets works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendOffsets("segment")
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$segment))
    expect_false(is.null(newMatches$segment.start))
    expect_false(is.null(newMatches$segment.end))
})

test_that("appendOffsets URL inference works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    
    ## ensure URL inference works
    attr(matches, "labbcat.url") <- NULL
    expect_true(is.null(attr(matches, "labbcat.url")))
    newMatches <- matches |> appendOffsets("segment")
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$segment))
    expect_false(is.null(newMatches$segment.start))
    expect_false(is.null(newMatches$segment.end))
    
})

test_that("appendOffsets works with multiple layers", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendOffsets(c("segment","syllables"))
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$segment))
    expect_false(is.null(newMatches$segment.start))
    expect_false(is.null(newMatches$segment.end))
    expect_false(is.null(newMatches$syllables))
    expect_false(is.null(newMatches$syllables.start))
    expect_false(is.null(newMatches$syllables.end))
})

test_that("appendOffsets works with count > 1", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendOffsets("segment", annotations.per.layer=2)
    ## get label/start/end - for aligned layer
    labels <- getMatchAlignments(
        labbcat.url, results$MatchId, "syllables", annotations.per.layer=2)
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$segment.1))
    expect_false(is.null(newMatches$segment.1.start))
    expect_false(is.null(newMatches$segment.1.end))
    expect_false(is.null(newMatches$segment.2))
    expect_false(is.null(newMatches$segment.2.start))
    expect_false(is.null(newMatches$segment.2.end))

})

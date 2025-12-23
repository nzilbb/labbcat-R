labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("appendLabels works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendLabels("phonemes")
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$phonemes))
})

test_that("appendLabels URL inference works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    
    ## ensure URL inference works
    attr(matches, "labbcat.url") <- NULL
    expect_true(is.null(attr(matches, "labbcat.url")))
    newMatches <- matches |> appendLabels("phonemes")
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$phonemes))
    
})

test_that("appendLabels works with multiple layers", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendLabels(c("phonemes","syllables"))
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$phonemes))
    expect_false(is.null(newMatches$syllables))
})

test_that("appendLabels works with count > 1", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    ## get a few matches
    matches <- getMatches(labbcat.url, "church")
    expect_true(nrow(matches) > 0)
    expect_false(is.null(attr(matches, "labbcat.url")))
    
    ## append labels
    newMatches <- matches |> appendLabels("phonemes", annotations.per.layer=2)
    ## get label/start/end - for aligned layer
    expect_equal(nrow(newMatches), nrow(matches))
    expect_false(is.null(newMatches$MatchId))
    expect_false(is.null(newMatches$phonemes.1))
    expect_false(is.null(newMatches$phonemes.2))
    expect_true(is.null(newMatches$phonemes.3))

})

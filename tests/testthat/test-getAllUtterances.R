labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getAllUtterances works ", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## a few participants:
    participant.ids <- getParticipantIds(labbcat.url)[1:3]
    
    ## get matches
    matches <- getAllUtterances(labbcat.url, participant.ids)

    ## check dataframe columns
    expect_true(length(matches$MatchId) > 3)
    expect_true(length(matches$Transcript) > 3)
    expect_true(length(matches$Line) > 3)
    expect_true(length(matches$LineEnd) > 3)
    expect_true(length(matches$Text) > 3)
    expect_true(length(matches$URL) > 3)

    ## ensure there's no word/segment information returned
    expect_true(is.null(matches$Target.word))
    expect_true(is.null(matches$Target.word.start))
    expect_true(is.null(matches$Target.word.end))
    expect_true(is.null(matches$Target.segment))
    expect_true(is.null(matches$Target.segment.start))
    expect_true(is.null(matches$Target.segment.end))

    ## check dataframe column types
    expect_false(is.numeric(matches$MatchId))
    expect_false(is.numeric(matches$Transcript))
    expect_true(is.numeric(matches$Line))
    expect_true(is.numeric(matches$LineEnd))
    expect_false(is.numeric(matches$Text))
    expect_false(is.numeric(matches$URL))
})


labbcat.url <- Sys.getenv('TEST_ADMIN_LABBCAT_URL')
username <- Sys.getenv('TEST_ADMIN_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_ADMIN_LABBCAT_PASSWORD')

test_that("fragmentLabels works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## Get a list of span annotations
    topics <- getMatches(
        labbcat.url, list("topic" = list(pattern = '.*quake.*', target = TRUE)))
    expect_true(nrow(topics) > 0)
    expect_false(is.null(topics$Transcript))
    expect_false(is.null(topics$Participant))
    expect_false(is.null(attr(topics, "labbcat.url")))
    
    ## Get the words and orthographies of the first view
    topics <- head(topics)
    newTopics <- topics |>
        fragmentLabels(
            c("word", "orthography"),
            start.column=Target.topic.start,
            end.column=Target.topic.end) 

    ## has the right columns
    expect_equal(nrow(newTopics), nrow(topics))
    expect_false(is.null(newTopics$MatchId))
    expect_false(is.null(newTopics$word))
    expect_false(is.null(newTopics$word.start))
    expect_false(is.null(newTopics$word.end))
    expect_false(is.null(newTopics$orthography))
    expect_false(is.null(newTopics$orthography.start))
    expect_false(is.null(newTopics$orthography.end))
})

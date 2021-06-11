labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchAlignments works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))
    
    ## get label/start/end
    labels <- getMatchAlignments(labbcat.url, results$MatchId, "topic")
    expect_equal(length(labels), 3)

    expect_equal(length(labels$topic), 3)
    expect_equal(length(labels$topic.start), 3)
    expect_equal(length(labels$topic.end), 3)
    topic <- as.vector(labels$topic)
    expect_equal(topic[[1]], "{September earthquake experience} ")
    expect_equal(topic[[2]], "{February earthquake experience}")
    expect_equal(topic[[3]], "{aftermath of the earthquakes}")

})

test_that("getMatchAlignments works with multiple layers", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))

    layers = c("topic", "phonemes")
    
    ## get label/start/end
    labels <- getMatchAlignments(labbcat.url, results$MatchId, layers)
    ## num layers * 3 fields (label/start/end)
    expect_equal(length(labels), length(layers) * 3)

    expect_equal(length(labels$topic), 3)
    expect_equal(length(labels$topic.start), 3)
    expect_equal(length(labels$topic.end), 3)
    topic <- as.vector(labels$topic)
    expect_equal(topic[[1]], "{September earthquake experience} ")
    expect_equal(topic[[2]], "{February earthquake experience}")
    expect_equal(topic[[3]], "{aftermath of the earthquakes}")

    expect_equal(length(labels$phonemes), 3)
    expect_equal(length(labels$phonemes.start), 3)
    expect_equal(length(labels$phonemes.end), 3)
    phonemes <- as.vector(labels$phonemes)
    expect_equal(phonemes[[1]], "kr2sJ3J")
    expect_equal(phonemes[[2]], "kr2sJ3J")
    expect_equal(phonemes[[3]], "kr2sJ3J")

})

test_that("getMatchAlignments works with count > 1", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7261;[0]=ew_0_7261",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7280;[0]=ew_0_7280"))

    ## get label/start/end
    labels <- getMatchAlignments(
        labbcat.url, results$MatchId, "phonemes", annotations.per.layer=2)
    ## 2 annotations per layer * 3 fields (label/start/end)
    expect_equal(length(labels), 6)

    expect_equal(length(labels$phonemes.1), 3)
    expect_equal(length(labels$phonemes.1.start), 3)
    expect_equal(length(labels$phonemes.1.end), 3)
    phonemes <- as.vector(labels$phonemes.1)
    expect_equal(phonemes[[1]], "kr2sJ3J")
    expect_equal(phonemes[[2]], "wQz")
    expect_equal(phonemes[[3]], "DEm")

    expect_equal(length(labels$phonemes.2), 3)
    expect_equal(length(labels$phonemes.2.start), 3)
    expect_equal(length(labels$phonemes.2.end), 3)
    phonemes <- as.vector(labels$phonemes.2)
    expect_equal(phonemes[[1]], "")
    expect_equal(phonemes[[2]], "wz")
    expect_equal(phonemes[[3]], "D@m")

})

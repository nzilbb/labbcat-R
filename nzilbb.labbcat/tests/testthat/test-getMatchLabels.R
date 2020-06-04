labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchLabels works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))
    
    ## get labels
    labels <- getMatchLabels(labbcat.url, results$MatchId, "topic")

    expect_equal(length(labels$topic), 3)
    topic <- as.vector(labels$topic)
    expect_equal(topic[[1]], "{September earthquake experience} ")
    expect_equal(topic[[2]], "{February earthquake experience}")
    expect_equal(topic[[3]], "{aftermath of the earthquakes}")

})

test_that("getMatchLabels works with multiple layers", {
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))

    layers = c("topic", "phonemes")
    
    ## get labels
    labels <- getMatchLabels(labbcat.url, results$MatchId, layers)
    expect_equal(length(labels), length(layers))

    expect_equal(length(labels$topic), 3)
    topic <- as.vector(labels$topic)
    expect_equal(topic[[1]], "{September earthquake experience} ")
    expect_equal(topic[[2]], "{February earthquake experience}")
    expect_equal(topic[[3]], "{aftermath of the earthquakes}")

    expect_equal(length(labels$phonemes), 3)
    phonemes <- as.vector(labels$phonemes)
    expect_equal(phonemes[[1]], "kr2sJ3J")
    expect_equal(phonemes[[2]], "kr2sJ3J")
    expect_equal(phonemes[[3]], "kr2sJ3J")

})

test_that("getMatchLabels works with count > 1", {
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7261;[0]=ew_0_7261",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7280;[0]=ew_0_7280"))

    ## get labels
    labels <- getMatchLabels(labbcat.url, results$MatchId, "phonemes", annotationsPerLayer=2)
    expect_equal(length(labels), 2)

    expect_equal(length(labels$phonemes.1), 3)
    phonemes <- as.vector(labels$phonemes.1)
    expect_equal(phonemes[[1]], "kr2sJ3J")
    expect_equal(phonemes[[2]], "wQz")
    expect_equal(phonemes[[3]], "DEm")

    expect_equal(length(labels$phonemes.2), 3)
    phonemes <- as.vector(labels$phonemes.2)
    expect_equal(phonemes[[1]], "")
    expect_equal(phonemes[[2]], "wz")
    expect_equal(phonemes[[3]], "D@m")

})

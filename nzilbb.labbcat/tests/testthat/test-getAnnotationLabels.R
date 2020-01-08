labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getAnnotationLabels works", {
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))
    
    ## get labels
    labels <- getAnnotationLabels(labbcat.url, results$MatchId, "topic", no.progress = T)

    expect_equal(length(labels$topic), 3)
    topic <- as.vector(labels$topic)
    expect_equal(topic[[1]], "{September earthquake experience} ")
    expect_equal(topic[[2]], "{February earthquake experience}")
    expect_equal(topic[[3]], "{aftermath of the earthquakes}")

})

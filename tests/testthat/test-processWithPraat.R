labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("processWithPraat works with default format measures", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    
    ## simulate some results
    results <- data.frame(
        MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
                  "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
                  "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"),
        Start=c(172.0, 322.0, 517.0),
        End=c(173.0, 323.0, 518.0))
    
    ## process with praat
    formants <- processWithPraat(
        labbcat.url,
        results$MatchId, results$Start, results$End,
        praatScriptFormants())

    expect_equal(length(formants$time_0_5), 3)
    expect_equal(length(formants$f1_time_0_5), 3)
    expect_equal(length(formants$f2_time_0_5), 3)
    expect_equal(length(formants$Error), 3)
    points <- as.vector(formants$time_0_5)
    expect_equal(points[1], 172.5)
    expect_equal(points[2], 322.5)
    expect_equal(points[3], 517.5)
})

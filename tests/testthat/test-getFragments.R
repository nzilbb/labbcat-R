labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getFragments works with vectors", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## simulate some results
    results <- data.frame(
        Transcript=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
        Line=c(10.0, 20.0, 30.5),
        LineEnd=c(11.0, 21.5, 31.0))
    
    ## define subdirectory
    subdir <- "test-getFragments"
    
    ## Get a list of fragments
    textgrid.files <- getFragments(
        labbcat.url, results$Transcript, results$Line, results$LineEnd, c("transcript","phonemes"),
        path=subdir) 

    expect_match(textgrid.files[[1]], "AP2505_Nelson__10.000-11.000.TextGrid")
    expect_match(textgrid.files[[2]], "AP2512_MattBlack__20.000-21.500.TextGrid")
    expect_match(textgrid.files[[3]], "AP2512_MattBlack__30.500-31.000.TextGrid")

    ## tidy up
    file.remove(textgrid.files)
    file.remove(subdir)
})

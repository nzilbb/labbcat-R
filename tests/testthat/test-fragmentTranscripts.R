labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("fragmentTranscripts works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## simulate some results
    results <- data.frame(
        Transcript=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
        Line=c(10.0, 20.0, 30.5),
        LineEnd=c(11.0, 21.5, 31.0))
    attr(results, "labbcat.url") <- labbcat.url
    
    ## define subdirectory
    subdir <- "test-fragmentTranscripts"
    
    ## Get a list of fragments
    textgrid.files <- results |> fragmentTranscripts(
        c("transcript","phonemes"), path=subdir) 

    expect_match(textgrid.files[[1]], "AP2505_Nelson__10.000-11.000.TextGrid")
    expect_match(textgrid.files[[2]], "AP2512_MattBlack__20.000-21.500.TextGrid")
    expect_match(textgrid.files[[3]], "AP2512_MattBlack__30.500-31.000.TextGrid")

    ## tidy up
    file.remove(textgrid.files)
    file.remove(subdir)
})

test_that("fragmentTranscripts URL inference and column selection works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ## simulate some results
    results <- data.frame(
        Transcript=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
        word.start=c(10.0, 20.0, 30.5),
        word.end=c(11.0, 21.5, 31.0))
    results$URL <- paste0(labbcat.url, "transcript?transcript=", results$Transcript)
    
    ## define subdirectory
    subdir <- "test-fragmentTranscripts"
    
    ## Get a list of fragments
    textgrid.files <- results |>
        fragmentTranscripts(
            c("transcript","phonemes"),
            start.column=word.start,
            end.column=word.end,
            path=subdir) 

    expect_match(textgrid.files[[1]], "AP2505_Nelson__10.000-11.000.TextGrid")
    expect_match(textgrid.files[[2]], "AP2512_MattBlack__20.000-21.500.TextGrid")
    expect_match(textgrid.files[[3]], "AP2512_MattBlack__30.500-31.000.TextGrid")

    ## tidy up
    file.remove(textgrid.files)
    file.remove(subdir)
})

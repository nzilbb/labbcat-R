labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("formatTranscript works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## get a list of transcripts
    transcript.ids <- getTranscriptIdsWithParticipant(labbcat.url, "AP2505_Nelson")
    
    ## define subdirectory
    subdir <- "test-formatTranscripts"
    
    ## Get a list of textgrids
    textgrid.files <- formatTranscript(labbcat.url, transcript.ids,
                                       c("utterance", "word"), path=subdir) 

    expect_match(textgrid.files[[1]], "test-formatTranscripts/AP2505_Nelson.TextGrid")

    ## tidy up
    file.remove(textgrid.files)
    file.remove(subdir)
})

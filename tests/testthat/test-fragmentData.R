labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("fragmentData works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
    
    ## get a word we know contains some mediapipe face images
    results <- getMatches(labbcat.url, "vivid")
    
    ## define subdirectory
    subdir <- "test-fragmentData"
    
    ## Get a list of data files
    image.files <- results |>
        fragmentData(
            "mediapipe",
            start.column=Target.word.start,
            end.column=Target.word.end,
            path=subdir)

    # some images returned
    expect_true(length(image.files) > 0)
    # we don't know how many, but they should be png files
    expect_true(endsWith(image.files[[1]], ".png"))

    ## tidy up
    file.remove(image.files)
    file.remove(subdir)
})

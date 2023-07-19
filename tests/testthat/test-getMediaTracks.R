labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getMediaTracks works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    tracks <- getMediaTracks(labbcat.url)
    expect_equal(length(tracks$description), 2)
    expect_true("Quake Face" %in% tracks$description)
    expect_true("_face" %in% tracks$suffix)
})

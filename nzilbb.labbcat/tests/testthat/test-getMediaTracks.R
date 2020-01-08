labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getMediaTracks works", {
    tracks <- getMediaTracks(labbcat.url)
    expect_equal(length(tracks$description), 2)
    expect_true("Quake Face" %in% tracks$description)
    expect_true("_face" %in% tracks$suffix)
})

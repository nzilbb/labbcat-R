labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getSerializerDescriptors works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    formats <- getSerializerDescriptors(labbcat.url)
    expect_true(length(formats$name) > 0)
    expect_true("text/praat-textgrid" %in% formats$mimeType)
})
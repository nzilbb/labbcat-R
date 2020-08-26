labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getDictionaries works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    dictionaries <- getDictionaries(labbcat.url)

    expect_true("CELEX-EN" %in% names(dictionaries))
    expect_true("Phonology (wordform)" %in% dictionaries$`CELEX-EN`)
})

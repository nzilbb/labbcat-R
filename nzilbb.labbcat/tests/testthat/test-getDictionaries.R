labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getDictionaries works", {
    dictionaries <- getDictionaries(labbcat.url)

    expect_true("CELEX-EN" %in% names(dictionaries))
    expect_true("Phonology (wordform)" %in% dictionaries$`CELEX-EN`)
})

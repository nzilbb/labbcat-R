labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getDictionaryEntries works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)",
                                    c("the", "quick", "brown", "fox"))

    expect_equal(length(entries), 5)
    expect_equal(names(entries), c("key", "V1", "V2", "V3", "V4"))
})

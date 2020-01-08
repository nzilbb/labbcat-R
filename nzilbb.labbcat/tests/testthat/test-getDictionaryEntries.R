labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getDictionaryEntries works", {
    entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)",
                                    c("the", "quick", "brown", "fox"))

    expect_equal(length(entries), 5)
    expect_equal(names(entries), c("key", "V1", "V2", "V3", "V4"))
})

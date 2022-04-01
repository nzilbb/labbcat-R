labbcat.url <- "http://localhost:8080/labbcat/"

test_that("loadLexicon works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    expect_true(is.null(
        loadLexicon(labbcat.url, "lexicon.txt", "unit-test", ",", "word,definition")))

    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("test-word"))
    expect_equal(length(entries), 3)
    expect_equal(nrow(entries), 1)
    expect_equal(names(entries), c("key", "V1", "V2"))
    expect_equal(nrow(subset(entries, key == "test-word")), 1)
})

test_that("getAnnotatorDescriptor works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## call FlatLexiconTagger.listLexicons
    FlatLexiconTagger <- getAnnotatorDescriptor(labbcat.url, "FlatLexiconTagger")

    ## we expect some info
    expect_false(is.null(FlatLexiconTagger))
    expect_false(is.null(FlatLexiconTagger$annotatorId))
    expect_false(is.null(FlatLexiconTagger$info))
    expect_false(is.null(FlatLexiconTagger$extApiInfo))
})

test_that("annotatorExt works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## call FlatLexiconTagger.listLexicons
    lexiconListJSON <- annotatorExt(labbcat.url, "FlatLexiconTagger", "listLexicons")

    ## we expect a JSON-encoded array
    expect_true("unit-test" %in% jsonlite::fromJSON(lexiconListJSON))
})

test_that("newLayer works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    unitTestLayer <- newLayer(labbcat.url, "unit-test", "Unit test layer", parent.id="word",
                              alignment = 2, annotator.id = "FlatFileDictionary",
                              annotator.task.parameters=paste(
                                  "tokenLayerId=orthography",
                                  "tagLayerId=test",
                                  "dictionary=unit-test:word->definition",
                                  sep="&"))
    expect_equal("unit-test", unitTestLayer$id)
    expect_equal("Unit test layer", unitTestLayer$description)
    expect_equal("word", unitTestLayer$parentId)
    expect_equal("string", unitTestLayer$type)
    expect_equal(2, unitTestLayer$alignment)
    
    unitTestLayer <- getLayer(labbcat.url, "unit-test")
    expect_equal("unit-test", unitTestLayer$id)
    expect_equal("Unit test layer", unitTestLayer$description)
    expect_equal("word", unitTestLayer$parentId)
    expect_equal(2, unitTestLayer$alignment)
})

test_that("saveLayer works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## get test layer
    unitTestLayer <- getLayer(labbcat.url, "unit-test")    
    expect_equal("string", unitTestLayer$type)
    expect_equal(2, unitTestLayer$alignment)
    
    ## edit layer
    unitTestLayer$type <- "ipa"
    unitTestLayer$alignment <- 0
    
    ## save layer
    unitTestLayer <- saveLayer(labbcat.url, unitTestLayer)
    ## returned object should have changes
    expect_equal("ipa", unitTestLayer$type)
    expect_equal(0, unitTestLayer$alignment)

    ## get layer from LaBB-CAT
    unitTestLayer <- getLayer(labbcat.url, "unit-test")
    ## returned object should have changes
    expect_equal("ipa", unitTestLayer$type)
    expect_equal(0, unitTestLayer$alignment)
})

test_that("addDictionaryEntry and removeDictionaryEntry works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")
    
    ## ensure there's no current definition
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("DictionaryEntry"))
    expect_true(is.null(entries$V1[[1]]))

    ## add a word to the layer dictionary
    expect_null(addDictionaryEntry(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition",
        "DictionaryEntry", "new-layer-entry-1"))
    
    ## now there's a definition
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("DictionaryEntry"))
    expect_false(is.null(entries$V1))

    ## remove the word
    expect_null(removeDictionaryEntry(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", "DictionaryEntry"))
    
    ## now there's no definition again
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("DictionaryEntry"))
    expect_true(is.null(entries$V1))
})


test_that("addLayerDictionaryEntry and removeLayerDictionaryEntry works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## ensure there's no current definition
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("LayerDictionaryEntry"))
    expect_true(is.null(entries$V1[[1]]))

    ## ensure the layer is configured
    unitTestLayer <- getLayer(labbcat.url, "unit-test")
    expect_equal(unitTestLayer$layer_manager_id, "FlatFileDictionary")

    ## add a word to the layer dictionary
    expect_null(addLayerDictionaryEntry(
        labbcat.url, "unit-test", "LayerDictionaryEntry", "new-layer-entry-1"))
    
    ## now there's a definition
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("LayerDictionaryEntry"))
    expect_false(is.null(entries$V1))

    ## remove the word
    expect_null(removeLayerDictionaryEntry(labbcat.url, "unit-test", "LayerDictionaryEntry"))
    
    ## now there's no definition again
    entries <- getDictionaryEntries(
        labbcat.url, "FlatFileDictionary", "unit-test:word->definition", c("LayerDictionaryEntry"))
    expect_true(is.null(entries$V1))

})

test_that("deleteLayer works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## layer is there
    expect_false(is.null(getLayer(labbcat.url, "unit-test")))
    ## delete layer returns no error
    expect_true(is.null(deleteLayer(labbcat.url, "unit-test")))
    ## layer is no longer there
    expect_true(is.null(getLayer(labbcat.url, "unit-test")))
    ## can't delete a nonexistent layer
    expect_false(is.null(deleteLayer(labbcat.url, "unit-test")))
})

test_that("deleteLexicon works", {    
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## delete lexicon
    expect_true(is.null(deleteLexicon(labbcat.url, "unit-test")))

    ## can't delete it again
    expect_false(is.null(deleteLexicon(labbcat.url, "unit-test")))
})

test_that("praatScriptIntensity default arguments works", {    
    script <- paste(
        "\nsubtractmean$ = \"yes\"", 
        "\nselect Sound 'sampleName$'",
        "\nTo Intensity: 100, 0, subtractmean$",
        "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
        "\nprint 'maxIntensity' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptIntensity(), script)
})

test_that("praatScriptIntensity with non-default arguments works", {    
    script <- paste(
        "\nsubtractmean$ = \"no\"", 
        "\nselect Sound 'sampleName$'",
        "\nTo Intensity: 90, 0.01, subtractmean$",
        "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
        "\nprint 'maxIntensity' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptIntensity(minimum.pitch = 90.0, time.step = 0.01, subtract.mean = F),
                 script)
})


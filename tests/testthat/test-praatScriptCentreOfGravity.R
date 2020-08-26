test_that("praatScriptCentreOfGravity default arguments works", {    
    script <- paste(
        "\nselect Sound 'sampleName$'",
        "\nfast$ = \"yes\"",
        "\nTo Spectrum: fast$",
        "\ncog_2 = Get centre of gravity: 2",
        "\nprint 'cog_2:0' 'newline$'",
        "\nRemove\n", sep="")
    expect_equal(praatScriptCentreOfGravity(), script)
})

test_that("praatScriptCentreOfGravity with powers works", {    
    script <- paste(
        "\nselect Sound 'sampleName$'",
        "\nfast$ = \"yes\"",
        "\nTo Spectrum: fast$",
        "\ncog_2 = Get centre of gravity: 2",
        "\nprint 'cog_2:0' 'newline$'",
        "\ncog_1 = Get centre of gravity: 1",
        "\nprint 'cog_1:0' 'newline$'",
        "\ncog_0_666666666666667 = Get centre of gravity: 0.666666666666667",
        "\nprint 'cog_0_666666666666667:0' 'newline$'",
        "\nRemove\n", sep="")
    expect_equal(praatScriptCentreOfGravity(powers = c(2,1,2/3)), script)
})

test_that("praatScriptCentreOfGravity without fast setting works", {    
    script <- paste(
        "\nselect Sound 'sampleName$'",
        "\nfast$ = \"no\"",
        "\nTo Spectrum: fast$",
        "\ncog_2 = Get centre of gravity: 2",
        "\nprint 'cog_2:0' 'newline$'",
        "\nRemove\n", sep="")
    expect_equal(praatScriptCentreOfGravity(spectrum.fast = F), script)
})


# nzilbb.labbcat package for R

The package provides functionality for querying and extracting data
from LaBB-CAT corpora, directly from R.

It currently provides access to quite basic data, but does
include a function for extraction sound fragments given a transcript
name and start/end times, so results CSV files can be processed
directy from R.

## Building the package

The package can be built from the source code using using:  
`./build.sh`

## Basic usage instructions

### Getting started

To install the latest version of the package in CRAN:

```{r install}
install.packages("nzilbb.labbcat")
```

To use it:

```{r require}
library(nzilbb.labbcat)
```

For all functions, the first parameter is the URL to the LaBB-CAT instance you want
to interact with - e.g. "https://labbcat.canterbury.ac.nz/demo/".

If the instance is password-protected, you'll be prompted for the username and password
the first time you invoke a function for that instance.

### Basic informational functions

There are some basic functions that provide information about the LaBB-CAT instance you're using.

```{r basic-info}
labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

id <- getId(labbcat.url)
layers <- getLayerIds(labbcat.url)
corpora <- getCorpusIds(labbcat.url)

paste("LaBB-CAT instance", id, "has", length(layers), "layers. The corpora are:")
corpora
```

### Accessing specific transcript and participant IDs

Transcripts are called 'graphs' because each one is represented as an 'Annotation Graph' (see Bird & Liberman 2001)

You can get a complete list of participants and transcripts:

```{r participants-graphs}
participants <- getParticipantIds(labbcat.url)
transcripts <- getGraphIds(labbcat.url)

paste("There are", length(participants), "participants. The first one is", participants[1])
paste("There are", length(transcripts), "transcripts. The first one is", transcripts[1])

```

There are also ways to get a filtered list of transcripts:

```{r listing-graph-ids}
## Transcripts in the UC corpus:
getGraphIdsInCorpus(labbcat.url, "UC")

## Transcripts featuring the participant QB1602:
getGraphIdsWithParticipant(labbcat.url, "QB1602")

## Transcripts with 'YW' in their name:
getMatchingGraphIds(labbcat.url, "id MATCHES '.*YW.*'")

```

### Accessing Media

Given a graph ID (i.e. a transcript name) you can access information about what media it has available:

```{r media}
## Default WAV URL:
getMedia(labbcat.url, "AP2515_ErrolHitt.eaf")

media <- getAvailableMedia(labbcat.url, "AP2515_ErrolHitt.eaf")

## All media file names
media$name

## Their URLs
media$url
```

Once you've got a URL, you can save its contents using the *httr* package, something like this:

```
install.packages("httr")

wav.file <- media$name[1]
wav.url <- media$url[1]

response <- httr::GET(wav.url, labbcat$authorization, httr::write_disk(wav.file, overwrite=TRUE), httr::progress())
if (httr::status_code(response) != 200) { # 200 means OK
  print(paste("Downloading", wav.file, "failed:", httr::http_status(response)$message))
} else {
  print(paste("Downloading", wav.file, "worked!"))
  
  ## Tidily delete the file we just downloaded
  file.remove(wav.file)
}
```

### Media fragments

You can access a selected fragment of a wav file with `getSoundFragments`. The function downloads a wav file to the current working directory, and returns the name of the file:

```{r media-fragment}
wav.file <- getSoundFragments(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0)

paste("The third 5 seconds is in this file:", wav.file)

## tidily delete the file we just downloaded
file.remove(wav.file)
```

`getSoundFragments` also accepts vectors for the `id`, `start`, and `end` parameters:

```{r media-fragments}
results <- data.frame(
  id=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
  start=c(10.0, 20.0, 30.0),
  end=c(15.0, 25.0, 35.0))

wav.files <- getSoundFragments(labbcat.url, results$id, results$start, results$end, no.progress = TRUE)

wav.files

## tidily delete the files we just downloaded
file.remove(wav.files)
```

This means that, if you have a results csv file exported from LaBB-CAT, which identifies segment tokens, you can iterate through the rows, downloading the corresponding wav files, something like:

```
## load the results from the CSV file
results <- read.csv("results.csv", header=T)

## download all the segment WAV files
wav.files <- getSoundFragments(
    labbcat, results$Transcript, results$segments.start, results$segments.end)
```

### Getting annotations from other layers

If you have search results in a CSV file, and would like to retrieve annotations from some other
layer, you can use the `getAnnotationLabels` function, providing the *MatchId* column (or the *URL* column) that indentifies the token, and the desuired layer name:

```
results <- read.csv("results.csv", header=T)
phonemes <- getAnnotationLabels(labbcat.url, results$MatchId, "phonemes")
```

### Search

Searching for matching tokens can be achieved using the `getMatches` function.

A basic search can be achieved with a simple, single-layer pattern like:

```
# all words starting with "ps..."
results <- getMatches(labbcat.url, list(orthography = "ps.*"))
```

More complex patterns, across multiple tokens an multiple layers, is possible by specifying
a more complex structure:

```
# the word 'the' followed immediately or with one intervening word by
# a hapax legomenon (word with a frequency of 1) that doesn't start with a vowel
results <- getMatches(labbcat.url, list(columns = list(
     list(layers = list(
            orthography = list(pattern = "the")),
          adj = 2),
     list(layers = list(
            phonemes = list(not = TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\{\\$@].*"),
            frequency = list(max = "2"))))))
```

The data frame that's returned contains columns that can be used as parameters for other functions:

```
# get all instances of the KIT vowel
results <- getMatches(labbcat.url, list(segments = "I"))

# get phonemic transcription for the whole word
phonemes  <- getAnnotationLabels(labbcat.url, results$MatchId, "phonemes")

# download all the segment WAV files
wav.files <- getSoundFragments(
    labbcat.url, results$Transcript, results$Target.segments.start, results$Target.segments.end)
```

### Looking up dictionaries

LaBB-CAT maintains a number of [dictionaries](https://labbcat.canterbury.ac.nz/demo/dictionaries) it uses to look things up.  These include access to CELEX, LIWC, and other lexicons that might be set up in the LaBB-CAT instance.

You can list the available dictionaries using:

```
dictionaries <- getDictionaries(labbcat.url)
```

With one of the returned layer manager ID and dictionary ID pairs, you can look up dictionary entries for a list of keys:

```
words <- c("the", "quick", "brown", "fox")
pronunciation <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)", words)
```

## Future Enhancements

### Getting annotations from other layers

Sometimes you might want annotations for the previous or next token, which might work like this:

```
previous.token.frequencies <- getAnnotationLabels(labbcat.url, results$MatchId, "frequency", token=-1)
next.token.frequencies <- getAnnotationLabelss(labbcat.url, results$MatchId, "frequency", token=1)
```

### Process with Praat

Another process that involves uploading a results CSV file is the *process with praat* option.  It would be good to be able to do this directly from R.

One tricky thing is that the *process with praat* page in LaBB-CAT has a bunch of options that would need to be specifiable somehow in the R function, including:
* the start and end time columns
* the window offset (surrounding context to extract)
* which measurements to make, including their options:
  * formats
    * F1, F2, and/or F3
    * how many sample points and where they should be in the interval
    * max formants for 'female' and 'male' participants
    * the exact Praat command to use
  * pitch
    * min, mean, and/or max
    * pitch floor for 'female' and 'male' participants
    * pitch ceiling for 'female' and 'male' participants
    * voicing threshold for 'female' and 'male' participants
    * the exact praat command to use
  * intensity maximum
    * the exact praat command to use
  * centre of gravity
    * p=2, p=1 and/or p=⅔

There's also a newish option on the page for specifying a custom script, which could look something like this:
```
# get centre of gravity and spread from spectrum
spectrum = To Spectrum... yes
# filter it
Filter (pass Hann band)... 1000 22000 100
# get centre of gravity
cog = Get centre of gravity... 2
# extract the result back out into a CSV column called 'cog'
print 'cog:0' 'newline$'
# tidy up objects
select spectrum
Remove
```

There are a few options for how to specify these from within R. The easiest would be to decide that only a limited set of options is initially available, something like this:

```
results <- read.csv("results.csv", header=T)
praat.results <- labbcat.processWithPraat(labbcat.url, 
  results$MatchId, results$segments.start, results$segments.end, window.offset=0.5, 
  formant.F1=TRUE, formant.F2=TRUE, 
  pitch.mean=TRUE)
```

Alternatively, or later on, more possible parameters could maybe be added:

```
praat.results <- labbcat.processWithPraat(labbcat.url, 
  results$MatchId, results$segments.start, results$segments.end, window.offset=0.5, 
  formant.F1=TRUE, formant.F2=TRUE, 
  formant.sample.points=c(0.25, 0.5, 0.75)), 
  formant.max.female=5000, formant.max.male=5500)
```

I'm not sure whether it makes sense to include the possibility of a custom Praat script, but if so, it could be another possible parameter:

```
## Read the Praat script from a file
script.file <- "coolStuff.praat""
script <- readChar(script.file, file.info(script.file)$size)

## Run the Praat script on each token
praat.results <- labbcat.processWithPraat(labbcat.url, 
  results$MatchId, results$segments.start, results$segments.end, window.offset=0.5, 
  praat.script=script)
```

### Retrieving transcript attributes

Something like: 

```
attributes <- getTranscriptAttributes(labbcat.url, id.list, c("language","duration"))
```

### Retrieving participant attributes

Something like: 

```
attributes <- getGraphAttributes(labbcat.url, participant.list, c("dob","gender"))
```


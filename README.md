# nzilbb.labbcat package for R

The package provides functionality for querying and extracting data
from LaBB-CAT corpora, directly from R.

It currently provides access to quite basic data, but does
include a function for extraction sound fragments given a transcript
name and start/end times, so results CSV files can be processed
directy from R.

## Building the package

The package can be built using:  
`./build.sh`

## Basic usage instructions

```
## connect to LaBB-CAT
labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/")

## Get the 5 seconds starting from 10s as a mono 22kHz file
wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
```

## Future plans

Future releases will include:
 * *getTranscriptAttributes* - for retrieving given attributes of a
 given set of transcripts.
 * *getParticipantAttributes* - for retrieving given attributes of a
 given set of participants.
 * *getGraph* - for retrieving annotations of a given transcript.
 * *search* - for searching for annotation patterns in the corpus.
 * *getDictionaryIds* and *lookup* - for accessing dictionary
 entries. 
 * *processWithPraat* - for batch acoustic measurement from search
 results. 

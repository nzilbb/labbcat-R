# SpectralCOG Finder
# Paul Warren, based on a script by Will Styler, originally written for Alec Buchner
# Adapted for LaBB-CAT batch processing by Robert Fromont
#
# This script takes the peak spectral frequency and spectral COG for tokens of /s/ and /esch/ 
# (i.e. 's' and 'S') in .wav/TextGrid pairs in the current directory. The TextGrid is assumed
# to have two tiers - 1 is a word tier and 2 is a phoneme tier.
#

# apply filter
Filter (pass Hann band): 550, 15000, 100
filtered$ = selected$ ("Sound")
To Spectrogram: 0.05, 15000, 0.002, 20, "Gaussian"
spectro$ = selected$ ("Spectrogram")

# start/end times, in seconds, relative to the beginning of the extracted window

start = targetStart
end = targetEnd

# Get points
p[1] = start + ((end - start) / 3)
p[2] = start + ((end - start) / 2)
p[3] = start + (2*((end - start) / 3))

# duration in seconds
duration = (end - start)
# duration in milliseconds
durationms = duration * 1000

# call peakmeasure and store the results in variables, for each point

for i from 1 to 3
    tp = p[i]
    call peakmeasure
    tp[i] = windowAbsoluteStart + tp
    storedf[i] = storedf
    storeda[i] = storeda
    freqm[i] = freqm	
    ampm[i] = ampm
    cog[i] = cog
    sk[i] = sk
endfor

# remove objects we created at the beginning
select Spectrogram 'spectro$'
Remove
select Sound 'filtered$'
Remove

# output CSV columns

print 'durationms' 'newline$'

# (unfortunately we can't do this in a loop)

print 'tp[1]' 'newline$'
print 'storedf[1]' 'newline$'
print 'storeda[1]' 'newline$'
print 'freqm[1]' 'newline$'
print 'ampm[1]' 'newline$'
print 'cog[1]' 'newline$'
print 'sk[1]' 'newline$'

print 'tp[2]' 'newline$'
print 'storedf[2]' 'newline$'
print 'storeda[2]' 'newline$'
print 'freqm[2]' 'newline$'
print 'ampm[2]' 'newline$'
print 'cog[2]' 'newline$'
print 'sk[2]' 'newline$'

print 'tp[3]' 'newline$'
print 'storedf[3]' 'newline$'
print 'storeda[3]' 'newline$'
print 'freqm[3]' 'newline$'
print 'ampm[3]' 'newline$'
print 'cog[3]' 'newline$'
print 'sk[3]' 'newline$'

procedure peakmeasure

	storeda = 0
	storedf = 0
	freqm = 0
	ampm = 0
	select Spectrogram 'spectro$'

	To Spectrum (slice):  'tp'
	slice$ = selected$ ("Spectrum")
	select Spectrum 'slice$'
	cog = Get centre of gravity... 2
	sk = Get skewness... 2
	select Spectrum 'slice$'
	To Ltas (1-to-1)
	ltas$ = selected$ ("Ltas")
	select Ltas 'ltas$'
	storeda = Get maximum: 1000, 0, "None"
	storedf = Get frequency of maximum: 1000, 0, "None"
	ampm = Get maximum: 2000, 7000, "None"
	freqm = Get frequency of maximum: 2000, 7000, "None"
		   
	select Ltas 'ltas$'
	Remove
	select Spectrum 'slice$'
	Remove
        
endproc

# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# area
if (! isGeneric("area"))
  setGeneric(
    name = "area",
    def = function(x, ...) standardGeneric("area")
  )

setMethod(
  f   = "area",
  sig = c(x = "Spectrum"),
  def = function(x, minHz = min(freq(x)), maxHz = max(freq(x)), scale = "dB", reference = NULL) {
    # Determine the indices of the spectrum that are included in the summation.
    .inds <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    # Initialize the vector of frequencies at which to sum values.
    .freqs <- frequencies(x)[.inds]
    # The point about discretizing the frequency scale that I was mentioned at our meeting
    # is that the value of the minHz argument might not be equal to the value of
    # frequencies(x)[.inds[1]]. For example, I just computed the spectrum of the sock.wav
    # example that I included with the tutorial, and its first Fourier frequency above 3000 Hz is
    # 3004.418. So, there’s an interval of the frequency scale that is 4.418 Hz in width that
    # is not being included in the computation. (The code below computes the bin width for
    # each sample, rather than using the binWidth() function that I mentioned at the meeting.)
    # To catch the bit at the low end of the [minHz, maxHz] interval:
    if (minHz < frequencies(x)[.inds[1]]) {
      .inds <- c(.inds[1]-1, .inds)
      .freqs <- c(minHz, .freqs)
    }
    # To catch the bit at the high end of the [minHz, maxHz] interval:
    .freqs <- c(.freqs, maxHz)
    # Compute bin widths
    .widths <- .freqs[2:length(.freqs)] - .freqs[1:(length(.freqs)-1)]
    # Set the scale of the spectrum.
    .vals <- values(x)
    if (scale == 'dB') {
      if (is.null(reference))
        .vals <- 10*log10(.vals)
      else if (reference == 'max')
        .vals <- 10*log10(.vals/max(.vals))
      else if (reference == 'min')
        .vals <- 10*log10(.vals/min(.vals))
      else if (is.numeric(reference))
        .vals <- 10*log10(.vals/reference)
      else
        message("Reference power must be argument when using dB scale.")
    } 
    .vals <- .vals[.inds]
    # Could also slot in some code here to set an effective dB floor relative to the peak...
    # Let me know if that’s something y’all want.
    # Compute area in each bin, and sum to get area in interval
    .area <- sum(.vals * .widths)
    return(.area)
  }
)







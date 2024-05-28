# refer https://github.com/birdcountindia/spectrograms/blob/main/abhisheka_spectrograms.R

# load package
library(seewave)
# # From github
# devtools::install_github("maRce10/warbleR")
# load package
library(warbleR)
library(tidyverse)


# downloading Andrew Spencer's recording https://macaulaylibrary.org/asset/168146691
download.file("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/168146691/",
              # https://stackoverflow.com/questions/35239128/downloading-sound-files-using-urls-in-r
              "whbw_song_spencer.mp3", mode = "wb")

# # didn't work out, poor quality spectrogram
# prep_static_ggspectro("whbw_song_spencer.mp3", savePNG = TRUE, onlyPlotSpec = FALSE, 
#                       colPal = c("white", "black"), crop = c(16, 18), colbins = 100,
#                       specHeight = 4, specWidth = 10, bgFlood = TRUE, noisereduction = NULL)


# plotting

png("spectrogram.png", width = 9, height = 4, units = "in", res = 300)

seewave::spectro(warbleR::read_wave("whbw_song_spencer.mp3"), 
                 tlim = c(0.5, 3.5), flim = c(0, 10), scale = FALSE,
                 collevels = c(-65:0), palette = reverse.gray.colors.1)

rect(2.08, 2.5, 2.55, 8.5, border = "darkred")
rect(1.27, 2, 1.67, 9.9, border = "darkblue")

text(2.3, 9, col = "darkred", cex = 0.8, 
     labels = expression(italic("trip-treeez")))
text(1.45, 1.7, col = "darkblue", cex = 0.8, 
     labels = expression(italic("tre-tre-tre")))

dev.off()

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

# downloading Sandip Das's recording https://macaulaylibrary.org/asset/168146691
download.file("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/546652551/",
              # https://stackoverflow.com/questions/35239128/downloading-sound-files-using-urls-in-r
              "whbw_call_das.mp3", mode = "wb")



# plotting

png("fig_spectrogram.png", width = 9, height = 8, units = "in", res = 300)

# Define the layout for two panels (one above the other)
layout(matrix(c(1, 2), ncol = 1), heights = c(1, 1))
# Adjust the margins to reduce the white border
par(mar = c(5, 5, 2, 2)) # Bottom, left, top, right margins in lines


seewave::spectro(warbleR::read_wave("whbw_song_spencer.mp3"), 
                 tlim = c(0.5, 3.5), flim = c(0, 10), scale = FALSE,
                 collevels = c(-65:0), palette = reverse.gray.colors.1,
                 axisX = FALSE) # suppress axis to create manually

axis(1, at = seq(0.5, 3.5, by = 0.5), labels = seq(0, 3, by = 0.5))

rect(2.08, 2.5, 2.55, 8.5, border = "darkred")
rect(1.27, 2, 1.67, 9.85, border = "darkblue")

text(2.3, 9, col = "darkred", cex = 0.8, 
     labels = expression(italic("trip-treeez")))
text(1.45, 1.7, col = "darkblue", cex = 0.8, 
     labels = expression(italic("tre-tre-tre-")))

# Add label "A" to the first plot
mtext("A", side = 3, line = 0, adj = -0.1, cex = 1.2, font = 2)


seewave::spectro(warbleR::read_wave("whbw_call_das.mp3"), 
                 tlim = c(4, 8), flim = c(0, 10), scale = FALSE, fastdisp = TRUE,
                 collevels = c(-65:0), palette = reverse.gray.colors.1,
                 axisX = FALSE) # suppress axis to create manually

axis(1, at = seq(4, 8, by = 1), labels = seq(0, 4, by = 1))

# abline(h = 8, col = "darkviolet")
text(6, 1, col = "darkviolet", cex = 0.9, 
     labels = expression(italic("trr-trr-trr-trr")))

# mtext("B", side = 1, outer = TRUE, adj = 0, cex = 1.2, font = 2)
mtext("B", side = 3, line = 0, adj = -0.1, cex = 1.2, font = 2)


dev.off()

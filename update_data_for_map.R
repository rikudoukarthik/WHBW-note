require(tidyverse)

ebd <- skimmr::read.ebd("data/ebd_spobuw2_relApr-2024.txt")

data_upd <- ebd %>% 
  distinct(OBSERVATION.DATE, LONGITUDE, LATITUDE) %>% 
  bind_rows(tribble(~OBSERVATION.DATE, ~LONGITUDE, ~LATITUDE,
                    "2024-05-21", 26.91797, 94.16748,
                    "2024-05-16", 27.596451, 95.365180,
                    "2024-05-18", 29.130871, 79.290562,
                    "2024-05-16", 30.055130, 80.205457,
                    "2024-05-18", 29.130871, 79.290562,
                    "2024-05-18", 29.130871, 79.290562
  ))

write_delim(data_upd, "data/whbw_latlong_incMay.txt")

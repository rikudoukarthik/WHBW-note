require(tidyverse)

ebd <- skimmr::read.ebd("data/ebd_spobuw2_relApr-2024.txt")

# erroneous records to remove
data_rm <- tribble(~OBSERVATION.DATE, ~LATITUDE, ~LONGITUDE,
                   "2022-04-23", 29.67797, 79.73785,
                   "2022-10-29", 30.77876, 77.83073,
                   "2023-04-30", 30.40782, 78.09791,
                   "2018-04-17", 30.39813, 79.03437,
) %>% 
  mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE),
         across(c("LATITUDE", "LONGITUDE"),
                ~ round(., 5)))



data_upd <- ebd %>% 
  mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE)) %>% 
  distinct(OBSERVATION.DATE, LONGITUDE, LATITUDE) %>% 
  # from species map
  bind_rows(tribble(~OBSERVATION.DATE, ~LATITUDE, ~LONGITUDE,
                    "2024-05-21", 26.91797, 94.16748,
                    "2024-05-16", 27.596451, 95.365180,
                    "2024-05-18", 29.130871, 79.290562,
                    "2024-05-16", 30.055130, 80.205457,
  ) %>% 
    mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE))) %>% 
  mutate(across(c("LATITUDE", "LONGITUDE"),
                ~ round(., 5))) %>% 
  # removing erroneous records
  anti_join(data_rm) %>% 
  # Chandu
  bind_rows(tribble(~OBSERVATION.DATE, ~LATITUDE, ~LONGITUDE,
                    "2024-03-09", 28.48016, 76.92002,
                    "2024-03-10", 28.48016, 76.92002,
                    "2024-03-16", 28.48016, 76.92002,
                    "2024-03-29", 28.48016, 76.92002,
                    "2024-04-14", 28.48016, 76.92002,
                    "2024-04-28", 28.48016, 76.92002,
                    "2024-05-04", 28.48016, 76.92002,
                    "2024-05-11", 28.48016, 76.92002,
                    "2024-05-19", 28.48016, 76.92002,
                    "2024-05-25", 28.48016, 76.92002,
  ) %>% 
    mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE),
           across(c("LATITUDE", "LONGITUDE"),
                  ~ round(., 5))))

write_delim(data_upd, "data/whbw_latlong_incMay.txt")
write_csv(data_upd, "data/whbw_latlong_incMay.csv")


# load("ebd_IN_relNov-2022.RData")
# 
# data <- data %>% filter(STATE %in% c("West Bengal", "Sikkim"))
# 
# save(data, file = "ebd_IN-WBSK_relNov-2022.RData")


load("ebd_IN-WBSK_relNov-2022.RData")
library(tidyverse)
library(lubridate)


### preparing data ####

data_IN <- data %>% 
  mutate(COUNTY_STATE = COUNTY)


preimp <- c("CATEGORY","EXOTIC.CODE","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","SPECIES.COMMENTS", "HAS.MEDIA")

nms <- names(read.delim("ebd_BT_relNov-2022.txt", nrows = 1, sep = "\t", header = T, 
                        quote = "", stringsAsFactors = F, na.strings = c(""," ", NA)))
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA
data_BT <- read.delim("ebd_BT_relNov-2022.txt", colClasses = nms, sep = "\t", header = T, 
                      quote = "", stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  mutate(COUNTY_STATE = STATE) %>% 
  # group ID and dates
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE))

nms <- names(read.delim("ebd_NP_relNov-2022.txt", nrows = 1, sep = "\t", header = T, 
                        quote = "", stringsAsFactors = F, na.strings = c(""," ", NA)))
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA
data_NP <- read.delim("ebd_NP_relNov-2022.txt", colClasses = nms, sep = "\t", header = T, 
                      quote = "", stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  mutate(COUNTY_STATE = STATE) %>% 
  # group ID and dates
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE))

### summaries ####

data0 <- data_IN %>% 
  bind_rows(data_BT) %>% 
  bind_rows(data_NP) %>% 
  filter(COUNTY_STATE %in% c("Darjeeling", "East Sikkim", "South Sikkim",
                             "Sarpang", "Zhemgang", "Samdrup Jongkha",
                             "Purwanchal"),
         MONTH %in% 6:8) %>% 
  mutate(COUNTY_STATE = factor(COUNTY_STATE, 
                               levels = c("Darjeeling", "East Sikkim", "South Sikkim",
                                          "Sarpang", "Zhemgang", "Samdrup Jongkha",
                                          "Purwanchal")))

data1 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>% 
  group_by(COUNTY_STATE) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data1


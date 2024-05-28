library(tidyverse)
library(terra)
# library(hrbrthemes) # ipsum theme
library(glue)
library(sf)
library(elevatr)
library(stars)
library(patchwork)


mutate_seasons <- function(data) {
  
  data <- data %>% 
    mutate(season = case_when(month %in% c(1,2,3,12) ~ "non-breeding",
                              month == 4 & day <24  ~  "non-breeding",
                              month == 11 & day >21 ~ "non-breeding",
                              
                              month == 5 ~ "passage",
                              month == 4 & day >=24 ~ "passage",
                              month == 6 & day <=12 ~ "passage",
                              month == 10 ~ "passage",
                              month == 9 & day >=28 ~ "passage",
                              month == 11 & day <=21 ~ "passage",
                              
                              
                              month %in% c(7,8) ~ "breeding",
                              month == 6 & day >12 ~ "breeding",
                              month == 9 & day <28 ~ "breeding"
    ))
  
  return(data)
  
}

mutate_aes <- function(data) {
  
  data <- data %>% 
    mutate(label = case_when(
      season == "non-breeding" ~ glue("Non-breeding\n(n = {n})"),
      season == "breeding" ~ glue("Breeding\n(n = {n})"),
      season == "passage" ~ glue("Passage\n(n = {n})")
    ),
    colour = case_when(
      season == "non-breeding" ~ "purple",
      season == "breeding" ~ "orange",
      season == "passage" ~ "white"
    ))
  
  return(data)
  
}



whbw <- read_csv("data/whbw_latlong_incMay.csv") %>% 
  mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE),
         DOY = yday(OBSERVATION.DATE)) %>% 
  # adding seasons
  mutate_seasons()


# distance to glacier data
whbw_glac <- read.csv("data/whbw_glac_distance.csv") %>% 
  mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE),
         DOY = yday(OBSERVATION.DATE)) %>% 
  # removing two duplicate rows
  dplyr::select(-c(fid, field_1)) %>%  
  distinct() %>% 
  # calculate distance in km
  mutate(dist_glac_km = dist_glac_m/1000) 


# elevation data
whbw_elev <- whbw_glac %>% 
  # mutate(across(c("LATITUDE", "LONGITUDE"),
  #               ~ round(., 3))) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs("WGS84")

# whbw_elev_rast <- whbw_elev %>% 
#   get_elev_raster(z = 8, clip = "bbox")
# save(whbw_elev_rast, file = "data/whbw_elev_raster.RData")
load("data/whbw_elev_raster.RData")


whbw <- whbw_elev %>% 
  # extracting elevation for our locations
  mutate(ELEVATION = whbw_elev_rast %>% 
           stars::st_as_stars() %>%
           stars::st_extract(whbw_elev) %>% # 4 rows (3 points) without elev info
           pull(1)) %>% 
  st_drop_geometry()



whbw_season <- whbw %>% 
  group_by(season) %>% 
  reframe(n = n()) %>% 
  mutate(season = factor(season, levels = c("non-breeding", "breeding", "passage"))) %>% 
  mutate_aes()

x_range <- tibble(DOY = -10:376) %>% # buffer of 11 days on either side
  mutate(OBSERVATION.DATE = as_date(DOY, origin = "2022-12-31"),
         month = month(OBSERVATION.DATE),
         month.lab = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE),
         day = day(OBSERVATION.DATE)) 

whbw_season_box <- x_range %>% 
  # adding seasons
  mutate_seasons() %>% 
  # splitting two passage
  mutate(season = case_when(
    season == "passage" & month < 8 ~ "passage1",
    season == "passage" & month > 8 ~ "passage2",
    season == "non-breeding" & month > 8 & DOY <= 0 ~ "non-breeding1",
    season == "non-breeding" & month < 8 & DOY >= 366 ~ "non-breeding2",
    season == "non-breeding" & month < 8 ~ "non-breeding1",
    season == "non-breeding" & month > 8 ~ "non-breeding2",
    TRUE ~ season
  )) %>% 
  group_by(season) %>% 
  reframe(XMIN = min(DOY),
          XMAX = max(DOY),
          XMED = median(DOY)) %>% 
  mutate(true.season = case_when(str_detect(season, "passage") ~ "passage",
                                 str_detect(season, "non-breeding") ~ "non-breeding",
                                 TRUE ~ season)) %>% 
  left_join(whbw_season, by = c("true.season" = "season"))


whbw <- whbw %>% 
  left_join(whbw_season_box, 
            by = c("season" = "true.season"),
            relationship = "many-to-many")

x_range <- x_range %>% 
  group_by(month.lab) %>%
  reframe(DOY = median(DOY)) %>%
  distinct(DOY, month.lab)



plot_glac <- ggplot() +  
  geom_rect(data = whbw_season_box,
            mapping = aes(xmin = XMIN, xmax = XMAX, ymin = -Inf, ymax = Inf,
                          group = true.season, fill = season),
            alpha = 0.3, fill = whbw_season_box$colour) +
  geom_label(data = whbw_season_box,
             mapping = aes(x = XMED, y = 900, label = label,
                           group = true.season, colour = season),
             fill = "black", colour = whbw_season_box$colour, 
             size = 3.5) +
  geom_point(data = whbw, 
             mapping = aes(DOY, dist_glac_km,
                           group = season, fill = colour),
             colour = "black", shape = 21, alpha = 0.5) +
  geom_smooth(data = whbw, 
              mapping = aes(DOY, dist_glac_km), 
              colour = "black",
              method = "loess", span = 0.6) +
  # 100 m line
  geom_hline(yintercept = 100, linetype = "longdash", alpha = 0.5) +
  scale_fill_identity() +
  labs(x = "Month", y = "Distance from glacier (km)") +
  scale_y_continuous(breaks = seq(0, 2000, by = 100),
                     limits = c(-20, 1000)) +
  scale_x_continuous(limits = c(-10, 376),
                     breaks = x_range$DOY,
                     labels = x_range$month.lab) +
  coord_cartesian(clip = "on", expand = FALSE) +
  theme(axis.ticks.x = element_line(colour = 'black', linewidth = 0.5),
        axis.ticks.length.x = unit(2, "mm"),
        axis.ticks.y = element_line(colour = 'black', linewidth = 0.5),
        axis.ticks.length.y = unit(2, "mm"),
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 11), 
        #axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black",
                                        linewidth = 1,
                                        fill = NA),
        plot.margin = margin(1, 1, 0.5, 0.5, unit = "cm"))

plot_elev <- ggplot() +  
  geom_rect(data = whbw_season_box,
            mapping = aes(xmin = XMIN, xmax = XMAX, ymin = -Inf, ymax = Inf,
                          group = true.season, fill = season),
            alpha = 0.3, fill = whbw_season_box$colour) +
  geom_point(data = whbw, 
             mapping = aes(DOY, ELEVATION,
                           group = season, fill = colour),
             colour = "black", shape = 21, alpha = 0.5) +
  geom_smooth(data = whbw, 
              mapping = aes(DOY, ELEVATION), 
              colour = "black",
              method = "loess", span = 0.6) +
  # 3000 m asl line
  geom_hline(yintercept = 3000, linetype = "longdash", alpha = 0.5) +
  scale_fill_identity() +
  labs(x = "Month", y = "Elevation (m asl)") +
  scale_y_continuous(breaks = seq(0, 5000, by = 500),
                     limits = c(-350, 5000)) +
  scale_x_continuous(limits = c(-10, 376),
                     breaks = x_range$DOY,
                     labels = x_range$month.lab) +
  coord_cartesian(clip = "on", expand = FALSE) +
  theme(axis.ticks.x = element_line(colour = 'black', linewidth = 0.5),
        axis.ticks.length.x = unit(2, "mm"),
        axis.ticks.y = element_line(colour = 'black', linewidth = 0.5),
        axis.ticks.length.y = unit(2, "mm"),
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 11), 
        #axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black",
                                        linewidth = 1,
                                        fill = NA),
        plot.margin = margin(1, 1, 0.5, 0.5, unit = "cm"))

plot_patch <- plot_glac / plot_elev &
  plot_annotation(tag_levels = "A")



ggsave(filename = "whbw_seasonality.jpeg", plot = plot_patch,
       device = "jpeg" , height = 30, width = 25, dpi = 300, units = "cm") 


library(tidyverse)
library(terra)
library(hrbrthemes)
whbw <- read.csv("whbw_latlong_incMay2024_final.csv")

whbw$OBSERVATION.DATE<-ymd(whbw$OBSERVATION.DATE)

whbw %>% mutate(month=month(whbw$OBSERVATION.DATE), day=day(whbw$OBSERVATION.DATE))

whbw <- whbw %>% mutate(month=month(whbw$OBSERVATION.DATE), day=day(whbw$OBSERVATION.DATE))


whbw_filtered <- whbw %>% mutate(season = case_when(month %in% c(1,2,3,12) ~ "non-breeding",
                                   month == 4 & day <24  ~  "non-breeding",
                                   month == 11 & day >21 ~ "non-breeding",

                                   month == 5 ~ "passage",
                                   month == 4 & day >=24 ~ "passage",
                                   month == 6 & day <12 ~ "passage",
                                   month == 10 ~ "passage",
                                   month == 9 & day >=28 ~ "passage",
                                   month == 11 & day <=21 ~ "passage",
                                   

                                   month %in% c(7,8) ~ "breeding",
                                   month == 6 & day >=12 ~ "breeding",
                                   month == 9 & day <28 ~ "breeding"
                                   ))
                                   
write.csv(whbw_filtered, "whbw_season_filtered.csv")


whbw_glac <- read.csv("whbw_glac_distance.csv")

whbw_glac$OBSERVATION.DATE<-ymd(whbw_glac$OBSERVATION.DATE)


whbw_glac_km <- whbw_glac %>% mutate(dist_glac_km = dist_glac_m/1000) %>% mutate(DayMonth = format(OBSERVATION.DATE, "%d-%m"))

whbw_glac_km_summary <- whbw_glac_km %>% group_by(month) %>% summarize(mean_dist_km = mean(dist_glac_km), sd_dist = sd(dist_glac_km), n = n_distinct(dist_glac_km))


plot <- ggplot(whbw_glac_km, aes(month, dist_glac_km)) +  
                            labs(x="MONTH", y="DISTANCE FROM GLACIER [KM]") +
                            scale_x_continuous(breaks=seq(1,12,by=1)) +
                            scale_y_continuous(breaks=seq(0,2000,by=100)) +

                            annotate("rect", xmin = 1, xmax = 4.8, ymin = -25, ymax = 700,
                             alpha = .3,fill = "white") + 
                            annotate("label", x=3, y=700, label="Non-Breeding (n=188)", fill="black", 
                              color="white",fontface="bold", size=4, vjust="inward") +

                            annotate("rect", xmin = 4.8, xmax = 6.5, ymin = -25, ymax = 700,
                             alpha = .3,fill = "purple") +
                            annotate("label", x=5.6, y=700, label="Passage (n=90)", fill="black", 
                              color="purple",fontface="bold", size=4, vjust="inward") +
                            
                            annotate("rect", xmin = 6.5, xmax = 9.8, ymin = -25, ymax = 700,
                             alpha = .3,fill = "orange") +
                            annotate("label", x=8, y=700, label="Breeding (n=51)", fill="black", 
                              color="orange",fontface="bold", size=4, vjust="inward") +

                            annotate("rect", xmin = 9.8, xmax = 11.6, ymin = -25, ymax = 700,
                             alpha = .3,fill = "purple") +

                            annotate("rect", xmin = 11.6, xmax = 12, ymin = -25, ymax = 700,
                             alpha = .3,fill = "white")  +
                            geom_smooth(colour="black") +
                            geom_point(colour="black") + 
            theme_ipsum_rc(axis_title_size = 12, 
  subtitle_size=14, 
  axis_title_face="bold", 
  plot_title_size = 18, 
  axis_title_just = "ct") +
theme(axis.ticks.x = element_line(colour = 'black', size = 0.5), 
  axis.ticks.length.x = unit(2, "mm"),
  axis.ticks.y = element_line(colour = 'black', size = 0.5), 
  axis.ticks.length.y = unit(2, "mm"), 
  legend.position="middle",
    #axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(colour = "black", size=1, fill=NA))



ggsave(filename="mean_monthly_distance_to_glacier_season.jpeg", plot=plot, device="jpeg" ,height=15, width=20,dpi=300,units="cm") 




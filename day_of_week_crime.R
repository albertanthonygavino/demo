setwd("~/Desktop/R_scripts")

library(dplyr)
library(readr)
library(lubridate)

coltypes <-list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

train <-read_csv(file="train.csv",col_types=coltypes)
test <-read_csv(file="test.csv",col_types=coltypes) 

train <-train %>% mutate(Year  = factor(year(Dates), levels=2003:2015),
         Month = factor(month(Dates), levels=1:12),
         Day   = day(Dates),
         Hour  = factor(hour(Dates), levels=0:23),
         dayDate = as.POSIXct(round(Dates, units = "days")),
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday")))

unique(train$Category)

mapdata <-
  train %>%
  filter(Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES FORCIBLE"))

mapdata %>%
  group_by(Category) %>%
  summarise(n=n())


library(ggplot2)
library(ggmap)

map<-get_map(location="sanfrancisco",zoom=12,source="osm")
# using google maps API
#map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")


#ggmap -------------------------------------------------

ggmap(map, extent='device', legend="topleft") +
  geom_point(aes(x=X, y=Y, colour=Category), data=mapdata ) +  
  ggtitle('Violent Crime in San Francisco')

ggmap(map, extent='device') +
  geom_point(aes(x=X, y=Y, colour=Category), data=mapdata ) +
  scale_colour_discrete(guide='none') +
  facet_wrap(~Category) +
  ggtitle('Violent Crime in San Francisco')

#contour plot -------------------------------------

contours <- stat_density2d(
  aes(x = X, y = Y, fill = ..level.., alpha=..level..),
  size = 0.1, data = mapdata, n=200,
  geom = "polygon")

ggmap(map, extent='device', legend="topleft") + contours +
  scale_alpha_continuous(range=c(0.25,0.4), guide='none') +
  scale_fill_gradient('Violent\nCrime\nDensity')+
  ggtitle('Violent Crime in San Francisco')


#model <- LiblineaR(train, target, type = 7, verbose = FALSE)
#rm(train)
#gc()
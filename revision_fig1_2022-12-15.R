library(tidyverse)
library(patchwork)
library(ggnewscale)
library(ggmap)
library(ggsn)
library(ggspatial)

ecu_epi <- read.csv(here::here("data", "ecuador_epi.csv"))

ecu_epi <- ecu_epi %>% 
  mutate(Year = case_when(DateCollected < "2013-12-31" ~"2013",
                          DateCollected < "2014-12-31" ~"2014",
                          DateCollected < "2015-12-31" ~"2015",
                          TRUE ~"CHECK")) 


register_google(key = "INSERT_YOUR_KEY_HERE")

map <- get_googlemap(center = "Ecuador", zoom = 7, maptype = "terrain", style = "feature:poi.business|visibility:off&style=feature:road|element:labels.icon|visibility:off&style=feature:transit|visibility:off") 

(ggmap(map) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  scalebar(x.min = -77, x.max = -75,
           y.min = -4.8, y.max = -4.7,
           dist = 100, 
           dist_unit = "km", 
           transform = T, 
           model = "WGS84",
           #st.bottom = F,
           st.dist = 1,
           height = 1, 
           st.size = 2) +
  annotation_north_arrow(location = "br", 
                         height = unit(0.3, "in"),
                         width = unit(0.3, "in"),
                         pad_x = unit(0.2, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  geom_point(data = ecu_epi, 
             aes(x = lon, y = lat, fill = LocationCode), 
             shape = 21, 
             color = "black", 
             size = 3) +   
  scale_fill_manual(values = loc_cols,
                    labels = locationLabels$LocationCode) +
  labs(fill = "Location") +
  theme(legend.position = "none")) +

(ecu_epi %>% 
  group_by(LocationCode, Year) %>% 
  tally() %>% 
  ggplot(aes(x = Year, y = n, fill = LocationCode)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = loc_cols,
                    labels = locationLabels$LocationCode) +
  background_grid(major = "xy", minor = "none") +
  labs(x = "Year", 
       y = "Number of samples",
       fill = "Location") +
  theme_minimal() + 
  theme(legend.position = "right") ) +

(ecu_epi %>% 
  group_by(LocationCode) %>% 
  tally() %>% 
  ggplot(aes(x = "", y = n, fill = LocationCode)) +
  geom_bar(width = 1, stat = "identity", position = "fill") +
  scale_fill_manual(values = loc_cols,
                    labels = locationLabels$LocationCode) +
  background_grid(major = "xy", minor = "none") +
  labs(#x = "Year", 
    y = "Number of samples",
    fill = "Location") +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  theme(legend.position = "none")) +
  
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect")

ggsave("viz/revised_fig1.png", width = 12, height = 6)

library(sf)
library(tidyverse)
library(rmapshaper)


counties <- USAboundaries::us_counties()
CONUS<- counties %>%
  filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico")) %>%
  st_transform(5070)

CONUS_border<- CONUS %>%
  st_union()
#filter to exeter
city<- USAboundaries::us_cities() %>%
  filter(stplfips_2010 %in% c( "0623126", "2737556"))


state<- counties %>%
  filter(state_name %in% c("Minnesota", "California")) %>%
  st_transform(5070) %>%
  st_union()

#get voronoi polygons

CONUS_tri<- CONUS %>%
  st_centroid(geoid) %>%
  st_union() %>%
  st_triangulate() %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(ID= 1:n())

#clip just to get the US
US_tri<- st_intersection(CONUS_tri,st_union(CONUS)) %>%
  mutate(ID= 1:n())

plot(US_tri)

ggplot()+
  geom_sf(data= CONUS_border, col="black", size=1)+
  geom_sf(data = US_tri, aes(col=ID), size =0.25)+
  scale_color_gradient(low = "blue", high = "light blue")+
  theme(legend.position = "none")+
  geom_sf(data = state, col="black", size =0.5, fill="red", alpha=.5)+
  geom_sf(data = city, size=4)+
  ggrepel::geom_label_repel(data=city,
      aes(label=city, geometry= geometry), stat = "sf_coordinates", size = 3, fill="light blue")+
  labs(title = "Home Of The Anez's")+
  ggsave(plot = last_plot(), file= "img/Anez.png")


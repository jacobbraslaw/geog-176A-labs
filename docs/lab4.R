#build a function to have everything ordered in a way that makes sense

nearest_state_plot= function(name){
state.of.interest = name

states = USAboundaries::us_states() %>%
  st_transform(5070)

#what are neighboring states and what is state.of.int

soi= states %>%
  filter(state_name == state.of.interest)

adjoining = st_filter(states, soi , .predicate = st_touches)



sample= st_make_grid(soi, n =70) %>%
  st_as_sf() %>%
  st_centroid()

plot(sample$x, pch=16, cex= 0.1)

closest = st_join(sample, adjoining, join= st_nearest_feature)
closest

ggplot()+
  geom_sf(data = closest, aes(color=state_name))


#casting puts it in the simplest form
voroni = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  st_as_sf()



v_state = st_join(voroni, closest) %>%
  st_cast()


combined<- v_state %>%
  group_by(state_name) %>%
  summarise() %>%
  st_intersection(soi)




ggplot()+
  geom_sf(data = adjoining, aes(fill=state_name))+
  geom_sf(data = combined, aes(fill= state_name), col= NA)+
  geom_sf(data = soi, col="black", fill=NA)+
  theme_minimal()+
  labs(fill="")

}


# function 2 ! make plots of tessalations , arg 1 is data, arg 2= name of plot

makeGGPlot = function(data, title) {

  ggplot()+
    geom_sf(data = data, fill= "white", col= "navy blue", size= 0.2)+
    labs(title= title, caption = paste("Number of features in tessalation", length(data$id)))+
    theme_void()
}

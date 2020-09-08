scenes<- getlandsat::lsat_scenes()

bb

down<- scenes %>%
  filter(min_lat<= bb$ymin, max_lat >= bb$ymax) %>%
  filter(min_lon<= bb$xmin, max_lon>= bb$xmax) %>%
  filter(as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "../data/palo_flood.csv")

meta= read_csv(file = "../data/palo_flood.csv")


#Map plot
#Only work in Marcos's laptop (by now)
keycode <- read_excel("MASTER copy survey2020 covid19.xlsx", 
                      sheet = "Survey questions & Key code")
Q5keycode<-slice(keycode, 228:283)
Q5keycode<-rename(Q5keycode, numbercode=URN, institution=`Unique Response Number`, city=MF_notes)
Q5keycode<-mutate(Q5keycode, numbercode=as.numeric(numbercode))


# get the coordinates -----------------------------------------------------

library(tmaptools) #Para el geocode_OSM

# geocode place names
coord_positions <- geocode_OSM(Q5keycode$city)
#delete NA positions
coord_positions<-coord_positions%>%
  filter(query!="NA")%>%
  rename(city=query)%>%
  dplyr::select(city, lat, lon)%>%
  mutate(EM="Yes")


# join em all ---------------------------------------------------------

d<-D%>%
  rename(numbercode=Q5)%>%
  left_join(., Q5keycode, by="numbercode")%>% #new column in d, institution
  left_join(., coord_positions, by="city")

#I left behind the 55 other (by now) ~35% of responses
extractothercities<-read_excel("MASTER copy survey2020 covid19.xlsx", 
           sheet = "Coded data")
sort(unique(extractothercities$Q5_a))
othercities<-c("Bilbao", "Gwynedd", "Helsinki", "Frankfurt", "Helmholtz", "Warszawa", 
               "Lisboa", "Las Palmas", "A Coruña", "Lovaina", "Leibniz", "Zadar",
               "Bremen", "Leiden", "Amsterdam", "Southampton","Plymouth", "Bremerhaven",
               "Santander", "Granada", "Madeira", "Vigo", "Lille", "Grenoble", "Dublin", "Aberdeen", "Bristol",
               "Cagliari", "Copenhagen", "Norwich", "Essex", "Gdansk", "Palermo", "Glasgow", "Wageningen", "Inverness")

coord_othercities <- geocode_OSM(othercities)
coord_othercities<-coord_othercities%>%
  filter(query!="NA")%>%
  rename(city=query)%>%
  dplyr::select(city, lat, lon)%>%
  mutate(EM="No")

world_map <- map_data("world")
# so call map of europe
# will change according to what you have in df
world_map = subset(world_map,long < max(d$lon, na.rm=T)+2 & long > min(coord_othercities$lon)-2)
world_map = subset(world_map,lat < max(d$lat, na.rm=T)+2 & lat > min(coord_othercities$lat)-2) #to include Las Palmas Canarias
# to get city names
LAB = world_map %>% 
  group_by(region) %>% 
  select(region,long,lat) %>% 
  summarise_all(mean)


#plot
ggplot(world_map, aes(x = long, y = lat)) + 
  geom_polygon(aes( group = group, fill = region),show.legend=FALSE,alpha=1/5) +
  geom_point(data = d, aes(x = lon, y = lat, color = EM),size = 1) + 
  geom_point(data = coord_othercities, aes(x = lon, y = lat, color = EM),size = 1) + 
  geom_text(data=d,aes(x = lon, y = lat, label=city),size=2)+
  scale_fill_grey() +theme_map()+
  theme(legend.position = "none")

###I'm going to remove Cape Town (sorry Louise!)
d<-filter(d, city!="Cape Town")

world_map <- map_data("world")
# so call map of europe
# will change according to what you have in df
world_map = subset(world_map,long < max(d$lon, na.rm=T)+2 & long > min(coord_othercities$lon)-2)
world_map = subset(world_map,lat < max(d$lat, na.rm=T)+2 & lat > min(coord_othercities$lat)-2) #to include Las Palmas Canarias

#plot
ggplot(world_map, aes(x = long, y = lat)) + 
  geom_polygon(aes( group = group, fill = region),show.legend=FALSE,alpha=.5) +
  geom_point(data = d, aes(x = lon, y = lat, color = EM),size = 1.5) + 
  geom_point(data = coord_othercities, aes(x = lon, y = lat, color = EM),size = 1.5) + 
  # geom_label_repel(data=d,aes(x = lon, y = lat, label=city),size=2)+
  scale_fill_grey()+
  coord_map(projection = "azequidistant")+
  OYSTERtheme+
  labs(x="", y="")+
  theme(
        panel.grid.major = element_line())
  

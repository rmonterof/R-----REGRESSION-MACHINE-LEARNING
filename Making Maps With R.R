# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

usa <- map_data("usa")
dim(usa)
head(usa)
tail(usa)

# Here is the high-res world map centered on the Pacific Ocean from mapdata

w2hr <- map_data("world2Hires")

dim(w2hr)
head(w2hr)
tail(w2hr)

# By default, geom_polygon() draws with no line color, but with a black fill:
  
usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
coord_fixed(1.3)

# Mess with line and fill colors
# Here is no fill, with a red line. Remember, fixed value of aesthetics go outside the aes function.

# coord_fixed()
ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)

# Here is violet fill, with a blue line.

gg1 <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "violet", color = "blue") + 
  coord_fixed(1.3)
gg1


# Adding points to the map
# Let's add black and yellow points at our lab and at the NWFSC in Seattle.

labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 2) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 1)

#group aesthetic
# Here we plot that map without using the group aesthetic:

ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat), fill = "violet", color = "blue") + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4) +
  coord_fixed(1.3)

# We can also get a data frame of polygons that tell us above state boundaries:

states <- map_data("state")
dim(states)

head(states)
tail(states)

#Plot all the states, all colored a little differently

# This is just like it is above, but we can map fill to region and make sure the the lines 
# of state borders are white.

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend



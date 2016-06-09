#experiment plotting shape data via ggmap function and do arbitary analysis
setwd("/home/vitidn/mydata/repo_git/RSpatial")

library(sp)
library(rgdal)
library(rgeos)
library(ggmap)

#sp_data is SpatialPolygonsDataFrame
sp_data <- readOGR(dsn="shape/Zoning", layer="Zoning") 

#slot names
slotNames(sp_data)
#boundary
sp_data@bbox
#number of polygons
nrow(sp_data@data)


# Reproject the data onto a "longlat" projection
sp_data = spTransform(sp_data, CRS("+proj=longlat"))

#find central lon,lat
centroid = gCentroid(sp_data)
mean_lon = coordinates(centroid)[,1]
mean_lat = coordinates(centroid)[,2]

#boundary by specifying zoom
#my_map = get_map(c(lon = mean_lon,lat = mean_lat),zoom = 13)
#boundary by speifying max & min lon/lat
#my_map = get_map(sp_data@bbox)
#center on aunt's house address
my_lon = -117.875279
my_lat = 33.845451
my_map = get_map(c(lon = my_lon,lat = my_lat),zoom = 14)

plotter = ggmap(my_map)
plotter

#transform shape data to use with ggmap
gg_data = fortify(sp_data)

#overlay with gg_data from the shape file
plotter +
  geom_polygon(aes(x = long, y = lat, group = group), data = gg_data,
               colour = 'white', fill = 'red', alpha = .4, size = .3)

#overlay the region that the house resides in as a green color
#overlay nearby area as a yellow color
#overlay further area as a red color
#draw a circle around the house
sp_point = SpatialPoints(matrix(c(my_lon,my_lat),nrow = 1),proj4string = CRS("+proj=longlat"))
#this index is the row index of polygon that contains aunt's house
index = which(gContains(sp_data,sp_point,byid = TRUE) == TRUE)
center_poly = sp_data[index,]
other_polys = sp_data[-index,]

centroids = gCentroid(other_polys,byid=TRUE)
distances = gDistance(sp_point,centroids,byid=TRUE)
other_polys@data["distance"] = distances

gg_center_poly = fortify(center_poly)
gg_near_poly = fortify( other_polys[which(other_polys@data[,"distance"] <= 0.01),] )
gg_far_poly = fortify( other_polys[which(other_polys@data[,"distance"] > 0.01),] )

plotter +
  geom_polygon(aes(x = long, y = lat, group = group), data = gg_center_poly,
               colour = 'white', fill = 'green', alpha = .4, size = .3) +
  geom_polygon(aes(x = long, y = lat, group = group), data = gg_near_poly,
               colour = 'white', fill = 'yellow', alpha = .4, size = .3)  +
  geom_polygon(aes(x = long, y = lat, group = group), data = gg_far_poly,
               colour = 'white', fill = 'red', alpha = .4, size = .2) +
  geom_point(aes(x = my_lon, y = my_lat), colour = "black", size = 50,fill = "black",alpha = .2)

#assign measure_value to each polygon and plot them to color gredient red <-> green
sp_data@data["measure_value"] = rnorm(nrow(sp_data)) 
rampFunc = colorRampPalette(c("red","green"))
colours = rampFunc(10)[as.numeric(cut( sp_data@data[,"measure_value"] ,breaks = 10))]
my_plot = plotter
for (c in unique(colours)){
  indexs = which(colours == c)
  print(nrow(sp_data[indexs,]))
  gg_data = fortify(sp_data[indexs,])
  my_plot = my_plot + geom_polygon(aes(x = long, y = lat, group = group), data = gg_data,
                                   colour = 'white', fill = c, alpha = 0.5, size = .2)
}
my_plot

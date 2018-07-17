# First try using osmar.

# Following the example at: https://journal.r-project.org/archive/2013-1/eugster-schlesinger.pdf

library("osmar")

# doesn't seem to work
src <- osmsource_api()
bb <- center_bbox(174.76778, -36.85056, 700, 700)
ua <- get_osm(bb, source = src)

# Next, try using osmdata, which seems to be a more up to date package
library(osmdata)
library(ggplot2)
library(geosphere)
library(sf)
library(dplyr)
q0 <- opq(bbox='Palmerston North, New Zealand')
q0a <- add_osm_feature(q0, key='highway')
q1 <- add_osm_feature(q0a, key='area', value = "!yes")
q2 <- add_osm_feature(q1, key='highway', value = "!cycleway|footway|path|pedestrian|steps|track|corridor|proposed|construction|bridleway|abandoned|platform|raceway|service")
q3 <- add_osm_feature(q2, key='motor_vehicle', value = "!no")
q4 <- add_osm_feature(q3, key='motor_car', value = "!no")
q5 <- add_osm_feature(q4, key='service', value = "!parking|parking_aisle|driveway|private|emergency_access")

# this actually works:
x <- osmdata_sf(q5)

# THE PROPER QUERY STRING. Not sure if this helps (basically it adds way["highway"] in case that's useful?
# instead of also having node[] and relation[] tags as well)
qs <- "[out:xml][timeout:25];\n(\n way[\"highway\"][\"area\"!=\"yes\"][\"highway\"!=\"cycleway|footway|path|pedestrian|steps|track|corridor|proposed|construction|bridleway|abandoned|platform|raceway|service\"] [\"motor_vehicle\"!=\"no\"] [\"motor_car\"!=\"no\"] [\"service\"!=\"parking|parking_aisle|driveway|private|emergency_access\"] (-40.516317,175.4512388,-40.196317,175.7712388);\n\n);\n(._;>;);\nout body;"

# for some reason the bounding box isn't set - I guess as we don't have it from q5?
x$bbox <- c(-40.516317,175.4512388,-40.196317,175.7712388)
x <- osmdata_sf(qs)

# Test! Looks like Palmy :)
ggplot(x$osm_lines) + geom_sf()

# Find the first and last point of each line
fl <- sf::st_coordinates(x$osm_lines) %>%
  as.data.frame %>%
  group_by(L1) %>%
  summarize(x1=first(X),
            x2=last(X),
            y1=first(Y), 
            y2=last(Y))

# Now find the bearings. NOTE: This just finds the bearings of each line segment, which is probably
# a bit wrong, as curved roads etc will only count start and end which isn't right at all.
# I guess for each one, we could instead construct the full bearing list and use that for some sort
# of weighted direction thingee? Though with enough roads it probably doesn't matter?
fl$bearings <- geosphere::bearing(as.matrix(fl[,c('x1', 'y1')]), as.matrix(fl[,c('x2', 'y2')])) %% 360

ggplot(fl, aes(x=bearings)) + geom_histogram(bins=30, boundary=0)

# Now switch this to polar coordinates
ggplot(fl, aes(x=bearings)) + geom_histogram(bins=30, boundary=0) +
  coord_polar() +
  scale_x_continuous(limits=c(0,360), breaks=seq(0,315,by=45))

# One problem with this is that the area of the pieces are no longer proportional: The
# area of each bar is proportional to the length squared. So we'll need to squareroot
# the counts:

ggplot(fl, aes(x=bearings)) + geom_histogram(bins=30, boundary=0, fill='steelblue', col='grey80', size=0.1) +
  coord_polar() +
  scale_x_continuous(name="", limits=c(0,360), breaks=seq(0,315,by=45)) +
#  scale_y_sqrt() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

# Now let's do the one in the github:
q0 <- opq(bbox='Santa Monica, California') %>%
  add_osm_feature(key='highway') %>%
  add_osm_feature(key='area', value = "!yes") %>%
  add_osm_feature(key='highway', value = "!cycleway|footway|path|pedestrian|steps|track|corridor|proposed|construction|bridleway|abandoned|platform|raceway|service") %>%
  add_osm_feature(key='motor_vehicle', value = "!no") %>%
  add_osm_feature(key='motor_car', value = "!no") %>%
  add_osm_feature(key='service', value = "!parking|parking_aisle|driveway|private|emergency_access")

x <- osmdata_sf(q0)

# check plot:
ggplot(x$osm_lines) + geom_sf()

# Doesn't seem quite right - the example has a much tighter match
fl <- sf::st_coordinates(x$osm_lines) %>%
  as.data.frame %>%
  group_by(L1) %>%
  summarize(x1=first(X),
            x2=last(X),
            y1=first(Y), 
            y2=last(Y))
fl$bearings <- geosphere::bearing(as.matrix(fl[,c('x1', 'y1')]), as.matrix(fl[,c('x2', 'y2')])) %% 360

ggplot(fl, aes(x=bearings)) + geom_histogram(bins=30, boundary=0, fill='steelblue', col='grey80', size=0.1) +
  coord_polar() +
  scale_x_continuous(name="", limits=c(0,360), breaks=seq(0,315,by=45)) +
  #  scale_y_sqrt() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

# try another one
q0 <- opq(bbox='Manhattan, NYC, NY, USA') %>%
  add_osm_feature(key='highway') %>%
  add_osm_feature(key='area', value = "!yes") %>%
  add_osm_feature(key='highway', value = "!cycleway|footway|path|pedestrian|steps|track|corridor|proposed|construction|bridleway|abandoned|platform|raceway|service") %>%
  add_osm_feature(key='motor_vehicle', value = "!no") %>%
  add_osm_feature(key='motor_car', value = "!no") %>%
  add_osm_feature(key='service', value = "!parking|parking_aisle|driveway|private|emergency_access")

x <- osmdata_sf(q0)

# check plot:
ggplot(x$osm_lines) + geom_sf()
# this is way too big. The bounding box looks about right, but it includes a bunch of stuff not in Manhattan.
# need to check what OSMnet is using for it's lookups, and if it's filtering things afterwards.

# Seems to be doing:
# 1. gdf_place = gdf_from_place(query, which_result=which_result, buffer_dist=buffer_dist)
# 2. polygon = gdf_place['geometry'].unary_union
# 3. graph_from_polygon()

# So need to replicate this - yay :)
# 1. Executes osm_polygon_download(query)
#  Then pulls a bunch of stuff out of the JSON response (bbox, geometry, place, constructing features)
#  Then from this creates a GeoDataFrame from the features and adds the default_crs and gdf_name
#  Then adds some buffer (by default 0), and returns the GeoDataFrame
# 2. Performs what I presume is a union on the geometry (by the looks the gdf_place is not used again, so we need
#  only the geometry).
# 3. Calls graph_from_polygon which is a different query to the above I think...
#  This buffers the polygon 500m and calls osm_net_download
#  It then creates a graph and truncates it back down to the buffered polygon.
#  And then simplifies it.
#  Then it truncates down to the unbuffered polygon (idea is to take care of intersections just outside)
# Finally, it works out streets per node in the buffered and updates the counts in the unbuffered
#
# osm_net_download:
#  Projects the geometry to lat/long
#  uses (poly:"{polygon}") instead of ({south},{west},{north},{east});
#
# osm_polygon_download(query)
#
#
# PROBLEM: NONE OF THE NZ CITIES HAVE BOUNDARY INFORMATION - It seems as though the best we get is 'region' (regional council?)
#
# You can see this when you go to OpenStreetMap and search for any NZ city
#
# SOLUTION: Use another data source than OSM to grab the polygon information?
#           e.g. use meshblocknz and then append the information from the statsNZ boundary files,
#                (suitably simplified)
#
# ALTERNATIVE: Can we use the street information from OSM to define the city by some sort of clustering?
#              Something like distance between intersections? Problem here is curves are points, but we
#              could fix that I guess by getting rid of curves first.
#              Seems quite a problem to solve though...


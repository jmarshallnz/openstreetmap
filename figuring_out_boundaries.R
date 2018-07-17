#
# Get boundary files from StatsNZ
#
library(meshblocknz)
library(dplyr)
mb2013 %>% select(AU2013_name)
mb2013 %>% names
mb2013 %>% pull(Ward2013_name) %>% table
mtu <- mb2013 %>% filter(RC2013_name %in% "Manawatu-Wanganui Region")
mtu %>% pull(Ward2013_name) %>% table
names(mtu)

mtu %>% pull(GC2013_name) %>% table

palmy <- mtu %>% filter(GC2013_name == "Palmerston North Constituency")
palmy %>% pull(TA2013_name) %>% table

palmy2 <- mb2013 %>% filter(TA2013_name == "Palmerston North City")
palmy2 %>% pull(GC2013_name) %>% table

# Now load in a meshblock shapefile
library(sf)
phu <- read_sf("~/data/R/epiclustR/shape_files/midcentral_phu.shp")
st_crs(phu) <- "+proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs"
#read_sf("~/Massey/Projects/EpiclustR/PrepareDatasets/MidCentral/Data/Meshblocks.shp")
plot_sf(phu[1])
palmy_phu <- phu %>% filter(MB06 %in% palmy$MB2006)
library(ggplot2)
ggplot(palmy_phu) + geom_sf()
palmy_phu2 <- phu %>% filter(MB06 %in% palmy2$MB2006)
ggplot(palmy_phu2) + geom_sf()

palmy_phu <- palmy_phu %>% left_join(palmy, by=c('MB06'='MB2006'))
ggplot(palmy_phu) + geom_sf(aes(fill=Ward2013_name))

palmy_final <- palmy_phu %>% filter(Ward2013_name != "Ashhurst-Fitzherbert Ward")
ggplot(palmy_final) + geom_sf(aes(fill=Ward2013_name))

# There doesn't really seem to be any decent way of getting these boundaries
pg <- sf::st_union(palmy_final)

# OK, now use this...
# Hmm, we'll need to convert to lat/long by first defining the CRS which for some reason isn't in the file.
pg_latlong <- st_transform(pg, 4326)
pg_coords <- st_coordinates(pg_latlong)
# now paste everything together
overpass_coords <- pg_coords %>% as.data.frame() %>% mutate(coord = sprintf("%.6f %.6f", Y, X)) %>%
  pull(coord) %>% paste(collapse=' ')

# Now do the osmdata stuff...
q0 <- opq(bbox='Palmerston North, New Zealand') %>%
  add_osm_feature(key='highway') %>%
  add_osm_feature(key='area', value = "!yes") %>%
  add_osm_feature(key='highway', value = "!cycleway|footway|path|pedestrian|steps|track|corridor|proposed|construction|bridleway|abandoned|platform|raceway|service") %>%
  add_osm_feature(key='motor_vehicle', value = "!no") %>%
  add_osm_feature(key='motor_car', value = "!no") %>%
  add_osm_feature(key='service', value = "!parking|parking_aisle|driveway|private|emergency_access")

# modify the string to use a polygon rather than the rest

opq_poly_string <- function(opq, poly_coords) {
  res <- NULL
  if (!is.null(opq$features)) {
    features <- paste(opq$features, collapse = "")
    features <- paste0(sprintf(" way %s (poly:\"%s\");\n", features, 
                               poly_coords))
    res <- paste0(opq$prefix, features, opq$suffix)
  }
  else if (!is.null(opq$id)) {
    id <- paste(opq$id$id, collapse = ",")
    id <- sprintf(" %s(id:%s);\n", opq$id$type, id)
    res <- paste0(opq$prefix, id, opq$suffix)
  }
  if (is.null(res)) 
    res <- paste0(opq$prefix, opq$suffix)
  return(res)
}

my_string <- opq_poly_string(q0, overpass_coords)
# righto, try this...
x <- osmdata_sf(my_string)

# Nice, we're getting close :)
ggplot(x$osm_lines) + geom_sf() + geom_sf(data=pg_latlong, fill='red', alpha=0.2)
ggplot(x$osm_points) + geom_sf() + geom_sf(data=pg_latlong, fill='red', alpha=0.2)

# Now we need to trim things a bit by the looks...
lines_int <- st_intersection(x$osm_lines, pg_latlong)
ggplot(lines_int) + geom_sf() + geom_sf(data=pg_latlong, fill='red', alpha=0.2)

# Woohoo - now do the histograms...
# First, ignoring length, and just doing the bearing from first->last (though allowing multilines)
fl <- lines_int %>% st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame %>%
  group_by(L2, L1) %>%
  summarize(x1=first(X),
            x2=last(X),
            y1=first(Y), 
            y2=last(Y))
fl$bearings <- geosphere::bearing(as.matrix(fl[,c('x1', 'y1')]), as.matrix(fl[,c('x2', 'y2')])) %% 360

# Looks great! :)
ggplot(fl, aes(x=bearings)) + geom_histogram(bins=30, boundary=0, fill='steelblue', col='grey80', size=0.1) +
  coord_polar() +
  scale_x_continuous(name="", limits=c(0,360), breaks=seq(0,315,by=45)) +
  #  scale_y_sqrt() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

# Now, including length. Basically just divide everything up as per standard? Hmmm...
fl <- lines_int %>% st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame %>%
  group_by(L2, L1) %>%

# Hmm, basically we want to use (x,y) and lead(x), lead(y) and run bearing() on them
  summarize(x1=X,
            x2=lead(X),
            y1=Y, 
            y2=lead(Y))

geoDist <- function(x, y) {
  m <- cbind(x,y)
  # BUG IN geosphere::distGeo - doesn't return NA and doesn't work with n=2 points...
  m2 <- m[-1,,drop=FALSE]
  m1 <- m[-nrow(m),,drop=FALSE]
  d <- geosphere::distGeo(m1, m2)
  c(d, NA)
}

fl2 <- lines_int %>% st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame %>%
  group_by(L2, L1) %>%
  mutate(bearing = geosphere::bearing(cbind(X, Y)) %% 360, dist = geoDist(X, Y)) %>%
  filter(!is.na(bearing))

# now a weighted histogram of bearing by dist
ggplot(fl2, aes(x=bearing, weight=dist)) + geom_histogram(bins=30, boundary=0, fill='steelblue', col='grey80', size=0.1) +
  coord_polar() +
  scale_x_continuous(name="", limits=c(0,360), breaks=seq(0,315,by=45)) +
  #  scale_y_sqrt() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

ggplot(fl2, aes(x=bearing)) + geom_histogram(bins=30, boundary=0, fill='steelblue', col='grey80', size=0.1) +
  coord_polar() +
  scale_x_continuous(name="", limits=c(0,360), breaks=seq(0,315,by=45)) +
  #  scale_y_sqrt() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

# and plot Palmy again to see
ggplot(lines_int) + geom_sf() + geom_sf(data=pg_latlong, fill='red', alpha=0.2)
# Very nice :)

# Note things aren't quite right still. Seems some tidying is still required on the borders?
ggplot(lines_int) + geom_sf()

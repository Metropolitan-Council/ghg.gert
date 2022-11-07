## Calculate average transit vehicle occupancy by Transit Market Area
# Joey Reid, <joey.reid@metrotransit.org>
library(data.table)
library(odbc)
library(sf)

## Load APC with odometer for 2018 ####
# the easy way: assign load to origin stop, intersect stops on TMAs
#   - works with missing data, distance traveled is always the difference in odometer readings between APC readings
#   - load will be assigned to the origin TMA only, should balance out if what goes up must come down.
#   - still need to watch out for odometer resets between stops
# the accurate way: assign load to stop segment, intersect segments with TMAs and assign load on proportion of intersection within TMA
#   - APC/AVL problems mean that we don't always have a record of every stop (or in the correct sequence),
#     so the reported stop segment may not match a scheduled segment, and there won't be a line segment to intersect with the TMA
tmdm = dbConnect(odbc(), "TMDatamart")
odbcSetTransactionIsolationLevel(tmdm, "read_uncommitted")
# TM odometer is 100s of feet
apc = data.table(dbGetQuery(tmdm, "SELECT calendar_id, vehicle_id, geo_node_id, message_time, odometer, board, alight, departure_load FROM passenger_count WHERE calendar_id BETWEEN ? AND ? AND odometer IS NOT NULL", params = list(120180101L, 120181231L)), key = c('calendar_id', 'vehicle_id', 'message_time', 'geo_node_id'))

# if odometer resets, assume it reset at the previous stop
apc[, `:=`(dist_mi = c(diff(odometer) / 52.8, NA_real_)), keyby = c('calendar_id', 'vehicle_id')]
apc[dist_mi < 0, `:=`(dist_mi = odometer / 52.8)]

geo_nodes = data.table(dbGetQuery(tmdm, "SELECT geo_node_id, COALESCE(mdt_longitude, map_longitude) * 1e-7 lon, COALESCE(mdt_latitude, map_latitude) * 1e-7 lat FROM geo_node"), key = 'geo_node_id')[!is.na(lon)]
dbDisconnect(tmdm)

## Load TMAs ####
gist = dbConnect(odbc(), "GISTransit")
odbcSetTransactionIsolationLevel(gist, 'read_uncommitted')
tmas = st_read(gist, query = "SELECT MarketArea, Shape.STAsBinary() Geometry FROM TransitMarketAreas", crs = 26915)
dbDisconnect(gist)

## Intersect ####
geo_nodes_sf = st_transform(st_as_sf(geo_nodes, coords = c('lon', 'lat'), crs = 4326), 26915)
gn_tma = st_intersection(geo_nodes_sf, tmas)
setDT(gn_tma)
gn_tma[, 'geometry' := NULL]

avo = gn_tma[apc[!is.na(dist_mi)], on = 'geo_node_id'][, .(.N, avo = weighted.mean(departure_load, dist_mi)), keyby = c('MarketArea')]
fwrite(avo, file.path('data', 'avo.csv'))

# Ashley Asmus wrote this for a different project
# Instead of using CMP segments for the bbox, we can use ctu geographies

# Setup -------------------------------------------------------------------
library(tidyverse)
library(osmdata)
library(dodgr)
library(sf)
library(stplanr)
library(foreach)
library(doParallel)
library(ggplot2)
# library(basemapR)

environmentIsLocked(asNamespace("curl"))
unlockBinding(sym = "has_internet", asNamespace("curl"))
assign(
  x = "has_internet",
  value = {
    function() {
      T
    }
  },
  envir = asNamespace("curl")
)
curl:::has_internet()


# ! replace with CTU geog
# Open CMP segment data
cmp_shp <- st_read("CMP_2018/CMP_2018_uni.shp") %>%
  st_transform(crs = 2811) %>%
  select(seg_id, CMPP_SI, name, ref)


# Get bounding box of CMP data
cmp_bbox <- cmp_shp %>%
  st_transform(crs = 4326) %>%
  st_bbox()

# Get osm_data_raw, using CMP bounding box as boundary.
osm_data_raw <- opq(cmp_bbox, timeout = 50) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Convert to polylines
osm_lines <- osm_poly2line(osm_data_raw)$osm_lines %>%
  select(osm_id, name, ref, highway) %>%
  st_transform(crs = 2811) %>%
  select(osm_id)

# Map of CMP segments:
cmp_shp_4326 <- st_transform(cmp_shp, crs = 4326)
osm_lines_4326 <- st_transform(osm_lines, crs = 4326)
osm_lines_4326_crop <- st_crop(osm_lines_4326, cmp_bbox)
# base <- base_map(cmp_bbox, increase_zoom = 2, basemap = "positron", nolabels = T)


# png('ProcessSummary2020/CMPPSegmentMap.png', height = 8, width = 8, units = 'in', pointsize = 16, bg = 'white', res = 300)
# ggplot()+
#   base +
#   geom_sf(data = cmp_shp_4326, color = councilBlue, lwd = 1)
# dev.off()


# png('ProcessSummary2020/OSM_CMPPSegment_Map.png', height = 8, width = 8, units = 'in', pointsize = 16, bg = 'white', res = 300)
# ggplot()+
#   base +
#   geom_sf(data = osm_lines_4326_crop, color = esBlue, lwd = 0.25, alpha = 0.5)+
#   geom_sf(data = cmp_shp_4326, color = councilBlue, lwd = 1)
# dev.off()

# Lake Street Example
# lakest_shp <- st_read('CMP_2018/CMP_2018_uni.shp') %>%
#   filter(seg_id == "1143")%>%
#   st_transform(crs = 2811) %>%
#   select(seg_id, CMPP_SI, name, ref)%>%
#   st_transform(crs = 4326)
# lakest_bbox <- c(xmin = -93.247576,ymin = 44.938942,xmax = -93.193374,ymax = 44.956548)
# lakest_base <- base_map(lakest_bbox, increase_zoom = 6, basemap = "positron", nolabels = F)
# osm_lines_lakest <- st_crop(osm_lines_4326, lakest_bbox)

# png('ProcessSummary2020/LakeStreetMap.png', height = 8, width = 8, units = 'in', pointsize = 16, bg = 'white', res = 300)
# ggplot()+
#   lakest_base +
#   geom_sf(data = osm_lines_lakest, color = esBlue, lwd = 0.25, alpha = 0.5)+
#   geom_sf(data = lakest_shp, color = councilBlue, lwd = 1)
# dev.off()


# # Conflation: CMP->OSM ----------------------------------------------------
# cores <- detectCores() - 1
# cl <- makeCluster(cores)
# registerDoParallel(cl)
#
# # pdf('test_osm_cmp_conflation.pdf', onefile=T)
# conflation_table <-
#   foreach(
#     a_cmpdat_row = 1:nrow(cmp_shp),
#     .combine = rbind,
#     .packages = c('sf', 'tidyverse', 'stplanr')
#   ) %dopar% {
#     # take one line from the CMP
#     cmp_line <- cmp_shp %>%
#       slice(a_cmpdat_row) %>%
#       st_cast("LINESTRING")
#
#     # sample many points along the line of interest
#     cmp_line_pts <- cmp_line %>%
#       st_line_sample(density = (1 / 25)) %>% st_cast(to = "POINT") # one every 25 meters
#
#     # draw a bounding box around the line points to speed this up
#     line_bbox <-
#       cmp_line %>%
#       st_buffer(dist = 500) %>% # to the nearest 500 m of the line
#       st_bbox()
#
#     # crop the highway dataset to that area
#     osm_lines_subset <- osm_lines %>%
#       st_crop(line_bbox)
#
#     # For each point, find the nearest OSM feature
#     nearest_osms <-
#       osm_lines_subset[(st_nearest_feature(cmp_line_pts, osm_lines_subset)), ] %>%
#       group_by(osm_id) %>%
#       tally() %>% # how many points from the CMP line fall along that OSM line?
#       filter(n > 1) %>%
#       unique() %>%
#       ungroup()
#
#     #Bounding box for the line, projected:
#     # map_bbox <-
#     #   cmp_line %>%
#     #   st_buffer(dist = 500) %>% # to the nearest 100 m of the line
#     #   st_transform(crs = 4326) %>%
#     #   st_bbox()
#
#     #Projected Points:
#     # cmp_line_pts <- st_transform(cmp_line_pts, crs = 4326)
#     # osm_lines_subset <- st_transform(osm_lines_subset, crs = 4326)
#     # cmp_line <- st_transform(cmp_line, crs = 4326)
#     # nearest_osms <- st_transform(nearest_osms, crs = 4326)
#
#     #Map:
#     # myplot<-
#     # ggplot()+
#     #   base_map(map_bbox, increase_zoom = 3, "positron", nolabels = F)+
#     #   # geom_sf(data = cmp_line_pts, alpha = 0.5, pch=1) +
#     #   geom_sf(data = osm_lines_subset, color = 'gray', lwd = 1)+
#     #   geom_sf(data = cmp_line, color = 'black', lwd = 4, alpha = 0.5)+
#     #   geom_sf(data = nearest_osms, aes(color = osm_id), lwd = 1) +
#     #   ggtitle(paste('CMP ', cmp_line$CMPP_SI))
#     # print(myplot)
#
#     # return a data frame
#     osm_result <- st_drop_geometry(nearest_osms) %>%
#       rename(n_points = n)
#     cmp_result <- st_drop_geometry(cmp_line) %>%
#       select(seg_id, CMPP_SI)
#
#     result_df <- merge(cmp_result, osm_result)
#     result_df
#   }
#
# dev.off()
# stopCluster(cl)
#
# conflation_table <-
#   read.csv('osm-cmp-conflation-table.csv') %>% select(-1)
# conflation_table$CMPP_SI <- as.factor(conflation_table$CMPP_SI)
# conflation_table$osm_id <- as.factor(conflation_table$osm_id)
#
# ALL JUNCTIONS: For each way in our conflated cmp-osm table, find the node IDs of all junctions ----------------------------------------------------
# One straightforward way to obtain junction densities is to convert the
# sf representation of osm_data_raw to a dodgr network, which is a simple data.frame
# with each row being a network edge.
osm_as_network <-
  osm_poly2line(osm_data_raw)$osm_lines %>% # the poly2line step converts strict sf polygons such as roundabouts into linestring objects,
  weight_streetnet(keep_cols = "highway", wt_profile = 1) %>% # wt_profile irrelevant here
  # the dodgr_contract_graph() call reduces the network to junction vertices only.
  dodgr_contract_graph()

# data frame version:
osm_as_dataframe <- as.data.frame(as.matrix(osm_as_network))
rm(osm_as_network)

osm_as_dataframe <- osm_as_dataframe %>% rename(intsct_way_type = highway)

# Pivot longer from an origin-destination matrix (from/to) to a long data frame of nodes/junctions/points
all_junctions <- osm_as_dataframe %>%
  # Only want the ways from our conflation table:
  rename(focal_way = way_id) %>%
  filter(focal_way %in% conflation_table$osm_id) %>%
  # Pivot longer - node IDs are from and to
  select(focal_way, from_id, to_id, from_lat, from_lon, to_lat, to_lon) %>%
  pivot_longer(
    c(from_id, to_id, from_lat, from_lon, to_lat, to_lon),
    names_to = c("fromorto", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  select(-fromorto) %>%
  rename(
    focal_way_node_id = id,
    focal_way_node_lat = lat,
    focal_way_node_lon = lon
  )

# ALL INTERSECTIONS: For each node, find all the ways that share it  ----------------------------------------------------
all_intersections <-
  all_junctions %>%
  # merging back:
  left_join(
    select(osm_as_dataframe, from_id, way_id, intsct_way_type),
    by = c("focal_way_node_id" = "from_id")
  ) %>%
  rbind(all_junctions %>%
    left_join(
      select(osm_as_dataframe, to_id, way_id, intsct_way_type),
      by = c("focal_way_node_id" = "to_id")
    )) %>%
  # at the very least, get rid of ways intersecting with themselves
  filter(!focal_way == way_id) %>%
  unique() %>%
  arrange(
    focal_way,
    focal_way_node_id,
    focal_way_node_lat,
    focal_way_node_lon
  ) %>%
  rename(intsct_way_id = way_id, intsct_way_type = intsct_way_type)


# TRUE INTERSECTIONS: Identify false intersections  ----------------------------------------------------
# Node junctions (all_intersections) contain both true intersections and places where segements of the same road meet.

# Set up a function to extract  all related ways for each focal way from the main result table
get_related_ways <- function(wayid) {
  res_subset <- conflation_table %>% filter(osm_id == wayid)

  # get the bounding box string for this way
  bboxstring <-
    cmp_shp %>%
    right_join(res_subset) %>%
    st_transform(crs = 4326) %>%
    st_bbox() %>%
    bbox_to_string()

  related_ways_query <-
    paste0(
      "[bbox:",
      bboxstring,
      "];\n",
      "way(id:",
      wayid,
      ");\n",
      "rel(bw);\n",
      # get all siblings
      "way(r);\n",
      # extract the way ID of all siblings
      "out;"
    )

  related_ways <- osmdata_sf(related_ways_query)
  related_ways <- related_ways$osm_lines
  related_ways <- related_ways$osm_id
  related_ways <- data.frame(cbind(related_ways, osm_id = wayid))
  write.csv(related_ways,
    paste0("related_ways/", wayid, ".csv"),
    row.names = F
  )
}

# Takes about 2 seconds per osm id running not in parallel
# tictoc::tic()
# get_related_ways(conflation_table$osm_id[2000])
# tictoc::toc()


# do this for all ways in the result table
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
# test_osms <- conflation_table %>% sample_n(100) %>% select(osm_id) # takes about 50 seconds per 100 osm ids (0.5 seconds per osm id)
# tictoc::tic()
# pdf('test_osm_cmp_conflation.pdf', onefile=T)
foreach(
  w = unique(conflation_table$osm_id),
  .packages = c("sf", "tidyverse", "stplanr", "osmdata", "bit64")
) %dopar% {
  environmentIsLocked(asNamespace("curl"))
  unlockBinding(sym = "has_internet", asNamespace("curl"))
  assign(
    x = "has_internet",
    value = {
      function() {
        T
      }
    },
    envir = asNamespace("curl")
  )
  curl:::has_internet()
  get_related_ways(w)
}
stopCluster(cl)
# tictoc::toc() # Takes about 1 hour to get all 10,000 way-relation tables.

# Related ways now live in separate .csv tables in the folder related_ways

# Remove false intersections  ----------------------------------------------------
remove_false_intersections <- function(a_focal_way_id) {
  # read in related way table for this way id
  these_related_ways <-
    data.table::fread(paste0("related_ways/", a_focal_way_id, ".csv"))
  these_true_intersections <- all_intersections %>%
    # Filter to just one focal_way id
    filter(focal_way == a_focal_way_id) %>%
    # Then filter out osm_ids joining to it that are in related_ways
    filter(!intsct_way_id %in% these_related_ways$related_ways)
  data.table::fwrite(
    these_true_intersections,
    paste0("true_intersections/", a_focal_way_id, ".csv"),
    row.names = F
  )
}

# Apply to all way ids in the table
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
tictoc::tic()
foreach(
  w = unique(all_intersections$focal_way),
  .packages = c("tidyverse", "data.table", "bit64")
) %dopar% {
  remove_false_intersections(w)
}
stopCluster(cl)
tictoc::toc() # Takes 23 minutes.

# Read data back in - true intersections
true_intersection_files <- list.files("true_intersections")
cores <- detectCores() - 1

cl <- makeCluster(cores)
registerDoParallel(cl)
tictoc::tic()
true_intersections <-
  data.table::rbindlist(
    foreach(
      f = true_intersection_files,
      .packages = c("data.table", "bit64")
    ) %dopar% {
      fread(paste0("true_intersections/", f))
    },
    use.names = FALSE
  )
stopCluster(cl)
tictoc::toc() # Takes 8 seconds

nrow(true_intersections) / nrow(all_intersections) # 74% of observations reamining - trimed 25%, nice!

# SUBSET INTERSECTIONS to roads only   ----------------------------------------------------
# Some open street map elements ("osm_data_raw" in the OSM lingo, confusingly) are little paths that don't count for us
# These should be removed from our list of elements.
non_roads <- c(
  "footway",
  "bridleway",
  "steps",
  "path",
  # a non-specific path - bridleway, footway, steps, cycleway
  "corridor",
  # a hallway inside of a building??
  "proposed",
  "track",
  # an agricultural or forestry road
  "sidewalk",
  "primary_link",
  "secondary_link",
  "tertiary_link",
  "motorway_link",
  "trunk_link"
)

intersections_subset <- true_intersections %>%
  filter(!intsct_way_type %in% non_roads)

nrow(intersections_subset) / nrow(true_intersections)
nrow(intersections_subset) / nrow(all_intersections)
# another 15% removed! Now only 60% of intersections remain.

# AGGREGATED INTERSECTIONS: Collapse intersections to highest-level road   ----------------------------------------------------
# many nodes are junctions between multiple roadway types.
data.table::uniqueN(intersections_subset$focal_way_node_id)

# 30,433 unique Nodes
nrow(intersections_subset) / data.table::uniqueN(intersections_subset$focal_way_node_id)
# 1.5 ways per node on average.

# For any intersection with multiple road types, we want to select the largest road
intersections_aggregated <- intersections_subset %>%
  mutate(intsct_way_type = factor(
    intsct_way_type,
    levels = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "residential",
      "living_street",
      "service",
      "construction",
      "pedestrian",
      "cycleway",
      "bus_guideway",
      "road",
      "unclassified",
      ""
    )
  )) %>%
  group_by(
    focal_way,
    focal_way_node_id,
    focal_way_node_lat,
    focal_way_node_lon
  ) %>%
  summarize(
    biggest_roadtype = first(levels(droplevels(intsct_way_type))),
    all_intsct_roadtypes = paste(unique(as.character(intsct_way_type)), collapse = ","),
    all_intsct_way_ids = paste(unique(as.character(intsct_way_id)), collapse = ",")
  ) %>%
  ungroup()


nrow(intersections_aggregated) / nrow(all_intersections) # Now only 48% of intersections remaining.

# CROPPED INTERSECTIONS: Remove intersections that fall outside of the CMP segment  ----------------------------------------------------
# When I re-conflated the CMP segments to OSM, there were many segments where the OSM line extended beyond
# that of the CMP. To remove these, get rid of any intersection that falls outside the lat/long range of the CMP segment.

# first need to rejoin to the CMP file:
intersections_x_cmp <-
  merge(conflation_table,
    intersections_aggregated,
    by.x = "osm_id",
    by.y = "focal_way"
  ) %>%
  select(
    CMPP_SI,
    seg_id,
    osm_id,
    focal_way_node_id,
    focal_way_node_lat,
    focal_way_node_lon,
    biggest_roadtype,
    all_intsct_roadtypes,
    all_intsct_way_ids
  ) %>%
  rename(node_lat = focal_way_node_lat, node_lon = focal_way_node_lon)


crop_nodes_function <- function(a_seg_id) {
  # Select subset of nodes to work with
  cmp_segment_nodes <-
    intersections_x_cmp %>%
    filter(seg_id == a_seg_id) %>%
    st_as_sf(crs = 4326, coords = c("node_lon", "node_lat"))
  # for a given seg_id, find its bounding box in the original dataset
  cmp_segment_line <-
    cmp_shp %>%
    filter(seg_id == a_seg_id)
  seg_bbox <-
    cmp_segment_line %>%
    st_transform(crs = 4326) %>%
    st_bbox(cmp_segment_line)

  # Crop to that bounding box
  intersections_cropped <- st_crop(cmp_segment_nodes, seg_bbox)
  # Remove geometry, return as data frame - nevermind
  # cmp_seg_cropped_nodes <- st_drop_geometry(cmp_seg_cropped_nodes)
  intersections_cropped
}

cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
tictoc::tic()

intersections_cropped <-
  foreach(
    s = unique(intersections_x_cmp$seg_id),
    .packages = c("tidyverse", "sf", "bit64"),
    .combine = rbind
  ) %dopar% {
    crop_nodes_function(s)
  }
stopCluster(cl)
tictoc::toc() # Takes 24 minutes


nrow(all_junctions)
# 117,532
nrow(all_intersections)
# 75,638
nrow(true_intersections)
# 56,074
nrow(intersections_subset)
# 44,995
nrow(intersections_aggregated)
# 36,427
nrow(intersections_x_cmp) # this grows in # of rows because multiple CMP segs share same intersections.
# 66,931
nrow(intersections_cropped)
# 51,682


st_write(intersections_cropped,
  dsn =
    "IntersectionsCropped", layer = "IntersectionsCropped",
  driver = "ESRI Shapefile",
  append = F
)


# MERGED INTERSECTIONS: Merge intersections that are within  50 m of each other   ----------------------------------------------------
# multi-lane roads can sometimes register as more than one intersection.
# for each intersection, draw a 50 m buffer diameter around it, then choose the
# largest road as the intersecting roadway type.
# Lanes are around 7-15 m wide
# https://gis.stackexchange.com/questions/102796/remove-points-within-x-distance

intersections_cropped <- intersections_cropped %>%
  mutate(biggest_roadtype = factor(
    biggest_roadtype,
    levels = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "residential",
      "living_street",
      "service",
      "construction",
      "pedestrian",
      "cycleway",
      "bus_guideway",
      "road",
      "unclassified",
      ""
    )
  ))

# Split intersections_cropped by seg_id (a CMP SI, directional)
inter_crop_spl <-
  group_split(intersections_cropped, seg_id, .keep = T)

intersections_merged <- list()

for (i in 1:length(inter_crop_spl)) {
  some_cropped_intersections <- inter_crop_spl[[i]]

  which_are_close <- some_cropped_intersections %>%
    st_transform(crs = 2811) %>%
    # all within 30 meters of each other:
    st_buffer(dist = 30) %>%
    st_intersects()
  which_are_close <- which_are_close %>%
    # this function collapses lists of nearby points,
    # so that if A is close to B, and B is close to C, A+B+C will be in one list together.
    # https://stackoverflow.com/questions/47322126/merging-list-with-common-elements
    map(function(element) {
      sort(unique(unlist(cross2(element, which_are_close, ~ length(intersect(.x, .y)) == 0))))
    })

  some_cropped_intersections$merge_list <- paste(which_are_close)


  some_merged_intersections <-
    some_cropped_intersections %>%
    group_by(merge_list, CMPP_SI, seg_id) %>%
    summarize(
      osm_id = first(osm_id),
      node_id = first(focal_way_node_id),
      biggest_roadtype = first(levels(droplevels(biggest_roadtype))),
      all_osm_ids = paste(unique(as.character(osm_id)), collapse = ","),
      all_node_ids = paste(unique(as.character(focal_way_node_id)), collapse = ","),
      all_intsct_roadtypes = paste(unique(as.character(
        all_intsct_roadtypes
      )), collapse = ","),
      all_intsct_way_ids = paste(unique(as.character(all_intsct_way_ids)), collapse = ","),
      geometry = st_centroid(st_union(geometry))
    ) %>%
    ungroup() %>%
    select(-merge_list)

  intersections_merged[[i]] <- some_merged_intersections
}

intersections_final <- bind_rows(intersections_merged)
# intersections_final$seg_id <- as.integer(intersections_final$seg_id)
# str(intersections_final)

intersections_final <- st_transform(intersections_final, crs = 2811)

st_write(intersections_final,
  dsn =
    "Intersections", layer = "Intersections",
  driver = "ESRI Shapefile",
  append = F
)


str(intersections_final)

intersections_polygon <- intersections_final %>%
  # all within 30 meters of each other:
  st_buffer(dist = 30) %>%
  st_cast("POLYGON")

st_write(intersections_polygon,
  dsn =
    "IntersectionsPolygon", layer = "IntersectionsPolygon",
  driver = "ESRI Shapefile",
  append = F
)


intersection_count <- intersections_final %>%
  st_drop_geometry() %>%
  group_by(CMPP_SI, seg_id, biggest_roadtype) %>%
  summarize(n_intersections = length(seg_id)) %>%
  ungroup() %>%
  arrange(desc(n_intersections)) %>%
  pivot_wider(
    names_from = biggest_roadtype, names_prefix = "roadtype_",
    values_from = n_intersections,
    values_fill = c(n_intersections = 0)
  ) %>%
  select_if(negate(function(col) is.numeric(col) && sum(col) == 0)) %>%
  arrange(CMPP_SI, seg_id)

intersection_count <- intersection_count %>%
  mutate(total_intersections = intersection_count %>% select(starts_with("roadtype_")) %>% rowSums())

# Some segments in the conflation table have no intersections! These look like highways
# (It worked!)
cmp_shp[!cmp_shp$seg_id %in% (intersections_final$seg_id), ][1:10, ]

# Union join back to our CMP data.
cmp_intdensity <- cmp_shp %>%
  full_join(intersection_count) %>%
  mutate_if(is.numeric, ~ replace_na(., 0))

cmp_intdensity <- cmp_intdensity %>%
  mutate(length = st_length(cmp_intdensity)) %>%
  mutate(length_mi = as.numeric(length / 1609)) %>%
  mutate(int_density = total_intersections / length_mi) %>%
  mutate(int_density_bin = cut(int_density,
    breaks = c(-0.0001, 2, 4, 8, 1000),
    labels = c("0-2", "2-4", "4-8", "8+")
  ))

# boxplot(cmp_intdensity$int_density)

# write this result to a shapefile
st_write(cmp_intdensity,
  dsn =
    "IntersectionDensity", layer = "IntersectionDensity",
  driver = "ESRI Shapefile",
  append = F
)

str(cmp_intdensity)

cmp_intdensity %>%
  st_drop_geometry() %>%
  write.csv("IntersectionDensity.csv")


# Another shapefile: Polygons
cmp_polygon <- cmp_intdensity %>%
  # all within 30 meters of each other:
  st_buffer(dist = 10) %>%
  st_cast("POLYGON")

cmp_polygon %>%
  slice(1) %>%
  plot()

st_write(cmp_polygon,
  dsn =
    "IntersectionDensityPolygon", layer = "IntersectionDensityPolygon",
  driver = "ESRI Shapefile",
  append = F
)

# Some mapping, segment by segment
nrow(all_junctions)
# 117,532
nrow(all_intersections)
# 75,638
nrow(true_intersections)
# 56,074
nrow(intersections_subset)
# 44,995
nrow(intersections_aggregated)
# 36,427
nrow(intersections_x_cmp) # this grows in # of rows because multiple CMP segs share same intersections.
# 66,931
nrow(intersections_cropped)
# 51,682
nrow(intersections_final)
# 32,808


a_line <- cmp_intdensity %>% filter(seg_id == "1143")

# Intersections
my_junctions <- all_junctions %>%
  mutate(
    focal_way_node_lat = as.numeric(as.character(focal_way_node_lat)),
    focal_way_node_lon = as.numeric(as.character(focal_way_node_lon))
  ) %>%
  left_join(conflation_table, by = c("focal_way" = "osm_id")) %>%
  filter(seg_id == a_line$seg_id) %>%
  st_as_sf(coords = c("focal_way_node_lon", "focal_way_node_lat"), crs = 4326)

my_intersections <- all_intersections %>%
  mutate(
    focal_way_node_lat = as.numeric(as.character(focal_way_node_lat)),
    focal_way_node_lon = as.numeric(as.character(focal_way_node_lon))
  ) %>%
  left_join(conflation_table, by = c("focal_way" = "osm_id")) %>%
  filter(seg_id == a_line$seg_id) %>%
  st_as_sf(coords = c("focal_way_node_lon", "focal_way_node_lat"), crs = 4326)

my_true_intersections <- true_intersections %>%
  mutate(
    focal_way_node_lat = as.numeric(as.character(focal_way_node_lat)),
    focal_way_node_lon = as.numeric(as.character(focal_way_node_lon)),
    focal_way = as.character(focal_way)
  ) %>%
  left_join(conflation_table, by = c("focal_way" = "osm_id")) %>%
  filter(seg_id == a_line$seg_id) %>%
  st_as_sf(coords = c("focal_way_node_lon", "focal_way_node_lat"), crs = 4326)


my_intersections_aggregated <- intersections_aggregated %>%
  mutate(
    focal_way_node_lat = as.numeric(as.character(focal_way_node_lat)),
    focal_way_node_lon = as.numeric(as.character(focal_way_node_lon)),
    focal_way = as.character(focal_way)
  ) %>%
  left_join(conflation_table, by = c("focal_way" = "osm_id")) %>%
  filter(seg_id == a_line$seg_id) %>%
  st_as_sf(coords = c("focal_way_node_lon", "focal_way_node_lat"), crs = 4326)


my_intersections_cropped <- intersections_cropped %>%
  filter(seg_id == a_line$seg_id)

my_intersections_merged <- intersections_final %>%
  filter(seg_id == a_line$seg_id) %>%
  st_transform(crs = 4326)

a_line <- st_transform(a_line, crs = 4326)

my_intersections_merged %>%
  group_by(biggest_roadtype) %>%
  tally()

# Lake Street Intersections:
# png('ProcessSummary2020/IntersectionLakeStreet.png', height = 6, width = 10, units = 'in', pointsize = 16, bg = 'white', res = 300)
# ggplot()+
#   lakest_base +
#   geom_sf(data = osm_lines_lakest, color = esBlue, lwd = 0.25, alpha = 0.5)+
#   geom_sf(data = a_line, color = councilBlue, lwd = 1) +
#   geom_sf(data = my_intersections_merged,
#           aes(color = biggest_roadtype), cex = 4, alpha = 0.5)
# dev.off()

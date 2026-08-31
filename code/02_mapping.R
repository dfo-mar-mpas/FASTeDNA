## code for making Figure 3 - maps

# load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(MarConsNetData)
library(maptiles)
library(terra)
library(patchwork)

# projections
latlong <- 4326
utm <- 32620

#source image cleaning function
source("https://raw.githubusercontent.com/dfo-mar-mpas/MCRG_functions/refs/heads/main/code/trim_img_ws.R")

# coordinates
map_coords <- read.csv("data/map_coords.csv") %>%
  st_as_sf(coords = c("long", "lat"), crs = latlong)

# St. Anns Bank MPA
sab <- read_sf("data/shapefiles/SAB_boundary_zones_2017.shp") %>%
  st_transform(latlong)

sab_nozones <- sab%>%
  st_transform(utm)%>%
  st_buffer(0.0025)%>% #this is small buffer that the vertex coordinate rounding issue
  st_union()%>% #gets rid of the zones
  st_transform(latlong)%>%
  st_as_sf()%>%
  rename(geometry=x)
  

#high resolution coastline for Nova Scotia
ns_coast <- read_sf("data/shapefiles/NS_coastline_project_Erase1.shp")%>%
            st_transform(latlong)

# ------------------------------------------------------------
# Basemap
# ------------------------------------------------------------

basemap <- ne_states(
  country = "Canada",
  returnclass = "sf"
) %>%
  dplyr::select(name_en, geometry) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(country = "Canada") %>%
  rbind(
    ne_states(
      country = "United States of America",
      returnclass = "sf"
    ) %>%
      dplyr::select(name_en, geometry) %>%
      st_union() %>%
      st_as_sf() %>%
      mutate(country = "United States of America")
  ) %>%
  st_transform(latlong)


# Higher-resolution lakes
lakes <- ne_download(
  scale = 10,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
) %>%
  st_transform(latlong)

#To get more detailed background images
get_basemap <- function(data, area, buffer_km = 20, zoom = 10,
                        provider = "Esri.WorldTopoMap", sab = NULL) {
  
  # Sampling sites
  area_data <- data %>%
    filter(Areas == area) %>%
    st_transform(3857) %>%
    dplyr::select(geometry)
  
  # Add additional spatial feature if supplied
  if (!is.null(sab)) {
    area_data <- rbind(
      area_data,
      sab %>% 
        st_transform(3857) %>%
        dplyr::select(geometry)
    )
  }
  
  # Define desired extent
  tile_extent <- area_data %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_buffer(buffer_km * 1000) %>%
    st_as_sf()
  
  # Download tiles
  basemap <- get_tiles(
    x = tile_extent,
    provider = provider,
    zoom = zoom,
    project = FALSE
  )
  
  # Crop to exact requested extent
  basemap <- terra::crop(
    basemap,
    terra::vect(tile_extent)
  )
  
  return(basemap)
}



# Manitoba
basemap_MB <- get_basemap(
  map_coords,
  "MB",
  buffer_km = 25,
  zoom = 10
)

# Nova Scotia

#add in St. Anns Bank to the mix as the 
extent_coords <- map_coords%>%
                 filter(Areas=="NS")%>%
                 dplyr::select(Areas)%>%
                 rbind(.,sab%>%
                        st_coordinates()%>%
                        data.frame()%>%
                        dplyr::select(X,Y)%>%
                        st_as_sf(coords=c("X","Y"),crs=latlong)%>%
                        rename(geometry=1)%>%
                        mutate(Areas="NS")%>%
                        dplyr::select(Areas,geometry))

basemap_NS <- get_basemap(
  extent_coords,
  "NS",
  buffer_km = 90,
  zoom = 8
)


# ------------------------------------------------------------
# Global map extent
# ------------------------------------------------------------

bound <- map_coords %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  rbind(
    sab %>%
      st_bbox() %>%
      st_as_sfc() %>%
      st_as_sf()
  ) %>%
  st_combine() %>%
  st_transform(utm) %>%
  st_buffer(100 * 1000) %>%       # 100 km buffer
  st_transform(latlong) %>%
  st_bbox()


# ------------------------------------------------------------
# Define zoomed areas
# ------------------------------------------------------------

# Create bounding boxes around each group of sites
# Function to create a bounding box around each area
get_area_bbox <- function(data, area, buffer_km = 20) {
  
  data %>%
    filter(Areas == area) %>%
    st_transform(utm) %>%
    st_buffer(buffer_km * 1000) %>%
    st_transform(latlong) %>%
    st_bbox()
}

# Get bounds for each area
bound_HB <- get_area_bbox(map_coords, "HB", 15)
bound_MB <- get_area_bbox(map_coords, "MB", 50)
bound_NS <- get_area_bbox(map_coords, "NS", 25)

# ------------------------------------------------------------
# All sites map
# ------------------------------------------------------------

p_allsites 

  ggplot() +
  geom_sf(data = basemap %>% filter(country == "Canada"), fill = "grey70") +
  geom_sf(data = basemap %>% filter(country == "United States of America"),fill="grey90") +
  geom_sf(
    data = lakes,
    fill = "white",
    colour = "black"
  ) +
  geom_sf(
    data = basemap,
    fill = NA,
    colour = "grey30"
  ) +
  geom_sf(
    data = map_coords,
    size = 2
  ) +
  geom_sf(
    data = sab,
    fill = NA,
    colour = "black"
  ) +
  coord_sf(
    expand = FALSE,
    xlim = bound[c(1,3)],
    ylim = bound[c(2,4)]
  ) +
  theme_bw()
  
  # ------------------------------------------------------------
  # Halifax Basin
  # ------------------------------------------------------------
  
  # Halifax Basin
  basemap_HB <- get_basemap(
    map_coords,
    area="HB",
    buffer_km = 4,
    zoom = 12
  )
  
  HB_plot <- ggplot() +
    layer_spatial(basemap_HB) +
    geom_sf(
      data = map_coords %>% filter(Areas == "HB"),
      size = 2
    ) +
    coord_sf(crs = 3857,expand=0) +
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.ticks=element_blank())+
    annotation_scale(location="br")
  
  ggsave("output/hb_plot.png",HB_plot,dpi=300,units="in")
  trim_img_ws("output/hb_plot.png")
    
  
  # ------------------------------------------------------------
  # Manitoba
  # ------------------------------------------------------------
  
  man_plot <- ggplot() +
    layer_spatial(basemap_MB) +
    geom_sf(
      data = map_coords %>% filter(Areas == "MB"),
      size = 2
    ) +
    coord_sf(crs = 3857,expand=0) +
    annotation_scale(location = "bl",text_cex = 0.5,
                     height = unit(0.15, "cm"))+
    theme_bw()
  
  ggsave("output/manplot.png",man_plot,dpi=300,units="in",height=12)
  trim_img_ws("output/manplot.png")
  
  # ------------------------------------------------------------
  # Nova Scotia Coast
  # ------------------------------------------------------------
  
  ns_plot <- ggplot() +
    layer_spatial(basemap_NS) +
    geom_sf(
      data = map_coords %>% filter(Areas == "NS"),
      size = 2
    ) +
    geom_sf(
      data = map_coords %>% filter(Areas == "HB"),
      size = 0.5
    ) +
    geom_sf(data=st_bbox(basemap_HB)%>%
                 st_transform(3857)%>%
                 st_as_sfc(),fill=NA)+
    geom_sf(
      data = sab,
      fill = NA,
      colour = "black"
    ) +
    coord_sf(crs = 3857, expand = 0, label_axes = "-NE-")+
    annotation_scale(location = "bl",text_cex = 0.5,
                     height = unit(0.15, "cm")) +
    theme_bw() 
  
  ggsave("output/nsplot.png",ns_plot,dpi=300,units="in",height=12)
  trim_img_ws("output/nsplot.png")
  
  
  #make the primary combination 
  
  bbox_mb <- st_bbox(basemap_MB)
  bbox_ns <- st_bbox(basemap_NS)  # swap for the true union extent if HB/sab reach beyond basemap_NS
  
  aspect_mb <- unname((bbox_mb["xmax"] - bbox_mb["xmin"]) / (bbox_mb["ymax"] - bbox_mb["ymin"]))
  aspect_ns <- unname((bbox_ns["xmax"] - bbox_ns["xmin"]) / (bbox_ns["ymax"] - bbox_ns["ymin"]))
  
  man_plot <- man_plot + theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"))
  ns_plot  <- ns_plot  + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 5, unit = "pt"))
  
  combined_plot <- man_plot + ns_plot +
    plot_layout(nrow = 1, widths = c(aspect_mb, aspect_ns))
  
  combined_plot <- combined_plot & theme(axis.text = element_text(size = rel(0.4)))
  
  ggsave("output/combined_map.png", combined_plot, dpi = 300, units = "in")
  trim_img_ws("output/combined_map.png")
  

  # ------------------------------------------------------------
  # Globe inset
  # ------------------------------------------------------------
  

  edna_bbox <- map_coords%>%
               st_bbox()%>%
               st_as_sfc()%>%
               st_buffer(5)%>% # degree buffer will trigger warning
               st_bbox()
  
  center_pt <- edna_bbox%>%
               st_set_crs(latlong)%>%
               st_as_sfc()%>%
               st_centroid()
  
  lon0 <- st_coordinates(center_pt)[1]
  lat0 <- st_coordinates(center_pt)[2]
  
  globe_crs <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s",lat0, lon0)
  
  #download the world globe basemap
  world_globe <- ne_countries(
    scale = "medium",
    returnclass = "sf"
  ) %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
    st_transform(globe_crs)
  
  
  
  #define the box denoting the study region you want to highlight
  global_box <- edna_bbox%>%
    st_set_crs(latlong)%>%
    st_as_sfc()%>%
    st_transform(globe_crs)
  
  mb_globe <- basemap_MB%>%
              st_bbox()%>%
              st_as_sfc()%>%
              st_transform(globe_crs)
 
  ns_globe <- ns_coast%>%
              st_bbox()%>%
              st_as_sfc()%>%
              st_transform(globe_crs)
  
  inset_centres <- rbind(mb_globe%>%st_centroid()%>%st_as_sf(),
                         ns_globe%>%st_centroid()%>%st_as_sf())
  
  #make a circle to wrap the globe plot
  
  globe_circle <- st_sfc(
    st_buffer(
      st_point(c(0, 0)),   # center of orthographic projection
      dist =  6378137  # meters
    ),
    crs = globe_crs
  )
  
  #crudgy way to make it so that the oceans are white in the plot
  globe_disc <- st_sfc(
    st_point(c(0, 0)),  # center in projected coords
    crs = globe_crs
  ) %>%
    st_buffer(dist = 6378137) %>%   # Earth radius in meters
    st_as_sf()
  
  
  global_inset <- ggplot() +
    geom_sf(data = globe_disc, fill = "white", colour = "black", linewidth = 0.4)+
    geom_sf(
      data = world_globe,
      colour = "grey20",
      linewidth = 0.2
    ) +
    geom_sf(
      data = world_globe%>%filter(formal_en == "Canada"),
      fill = "grey60",
      colour = "grey20",
      linewidth = 0.2
    ) +
    geom_sf(
      data = global_box,
      fill = NA,
      colour = "black",
      linewidth = 0.9
    ) +
    geom_sf(data=inset_centres)+
    # geom_sf(
    #   data = mb_globe, ## too small to see on the map
    #   fill = NA,
    #   colour = "black",
    #   linewidth = 0.9
    # ) +
    # 
    # geom_sf(
    #   data = ns_globe,
    #   fill = NA,
    #   colour = "black",
    #   linewidth = 0.9
    # ) +
    geom_sf(data = globe_circle,
            fill = NA,
            colour = "grey30",
            linewidth = 0.4)+
    coord_sf(crs = globe_crs) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA)
    )
  
  ggsave(
    "output/global_inset.png",
    plot = global_inset,
    width = 4,
    height = 4,
    dpi = 600,
    bg = "transparent"
  )
  
  # ------------------------------------------------------------
  # Figure 6 Basin mosaic inset
  # ------------------------------------------------------------
  
  basin_inset <- get_area_bbox(map_coords%>%filter(site=="BIO - Floating Marina"), "HB", 3)
  
  basemap_HB_hr <- get_basemap(
    map_coords%>%filter(site=="BIO - Floating Marina"),
    area="HB",
    buffer_km = 0.25,
    zoom = 18
  )
  
  p1_hb <- ggplot() +
    layer_spatial(basemap_HB_hr) +
    geom_sf(
      data = map_coords%>%filter(site=="BIO - Floating Marina"),
      size = 2
    ) +
    coord_sf(crs = 3857,expand=0) +
    theme_bw()+
    annotation_scale(location="br")
  
  ggsave("output/hb_zoom.png",p1_hb,height=6,width=6,units="in",dpi=300)
  trim_img_ws("output/hb_zoom.png")
  
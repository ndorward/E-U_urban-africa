setwd("~/Dropbox/How urban is Africa/Data")

library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)
library(tidyr)
library(rmapshaper)
library(gtools)
library(foreach)

#define getmap function for plotting ggmap and geom_sf https://github.com/dkahle/ggmap/issues/160 
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

#read africa gadm2 data
africa = sf::st_read("gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH",
                             "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MWI",
                             "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "SWZ", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF",
                             "ZMB", "ZWE")) %>% #read African countries excluding small islands
  rmapshaper::ms_simplify(., keep = 0.1, keep_shapes = TRUE)

africa2 = sf::st_read("gadm36_levels_shp/gadm36_2.shp") %>%
  dplyr::filter(GID_0 %in% africa$GID_0)

#define function for getting Tryptich plots - currently reads for year 2020

get_tryptich_plot = function(country_iso, city_lon, city_lat, city_name, year) {
  
  #register google api key 
  register_google(key = "AIzaSyCfof_SFJ1IR5nr4TEBzgtOhBqt0TXmFX8")
  
  string_pattern = c(country_iso, year, '.shp$')
  
  results_list = list.files(path = "urban_boundaries", pattern=paste(string_pattern, collapse=".+"),
                            full.names=FALSE) %>%
    gtools::mixedsort() %>%
    #keep 400, 1200, 2000
    .[c(2, 6, 11)]; 
  
  setwd("./urban_boundaries")
  results_list_df = lapply(results_list, st_read)
  setwd("~/Dropbox/How urban is Africa/Data")
  
  
  baselayer = get_googlemap(center = c(lon = city_lon, lat = city_lat), 
                            zoom = 10, scale = 2, maptype ='terrain', color = 'bw') 
  
  #apply getmap function
  myMap = ggmap_bbox(baselayer)
  
  #convert baselayer to ggmap for bounding box
  baselayer = baselayer %>% ggmap()
  
  #create bounding box
  bb = baselayer[[1]] %>% 
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_bbox()
  
  gadm_base = st_crop(africa2, bb) %>%
    st_transform(., 3857)
  
  cropped_df = list()
  foreach (i = 1:length(results_list_df), j = 1:length(results_list)) %do% {
    
    df = results_list_df[[i]] %>%
      st_crop(., bb) %>%
      dplyr::mutate(density_threshold = results_list[[j]]) %>%
      st_transform(., 3857) %>%
      #apply min pop threshold
      dplyr::filter(sttlmn_ > 50000)
    
    cropped_df[[i]] = df
    
  }
  
  plot_data = do.call("rbind", cropped_df) %>%
    dplyr::mutate(density_threshold = tidyr::extract_numeric(density_threshold)) %>%
    dplyr::mutate(density_threshold = stringr::str_remove(density_threshold, year)) %>%
    mutate(across(density_threshold, factor, levels=c("400","1200","2000")))
  
  plot = ggmap(myMap) +
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = plot_data, fill = "blue", alpha =0.1, lwd = 0, inherit.aes = FALSE) +
    geom_sf(data = gadm_base, fill = "white", alpha = 0, lwd = .1, inherit.aes = FALSE) + 
    facet_wrap(~ density_threshold) +
    ylab("") + xlab("") + 
    ggtitle(paste(city_name, ", ", country_iso, sep = ""))
  
  return(plot)

}

#write out for each country
country_list = as.list(c("GHA", "NGA", "RWA", "MWI", "TZA", "ZMB"))
lat_list = as.list(c(5.614818, 6.631663, -1.935114, -13.96692, -6.776012, -15.416667))
long_list = as.list(c(-0.205874, 3.311414, 30.082111, 33.78725, 39.178326, 28.283333))
city_list = as.list(c("Accra", "Lagos", "Kigali", "Lilongwe", "Dar es Salam", "Lusaka"))

#loop to write these to file 
local_plots = list()
foreach(i = 1:length(country_list), j = 1:length(long_list), k = 1:length(lat_list), l = 1:length(city_list)) %do% {
  
  plot = get_tryptich_plot(country_list[[i]], long_list[[j]], lat_list[[k]], city_list[[l]], year = "2020")
  
  local_plots[[i]] = plot
  
  ggsave(plot, file = paste("tryptich_density_plots_", country_list[[i]], "_", city_list[[l]], ".pdf", sep = ""), 
         dpi = 400, width = 12, height = 10, 
         path = "./results_plots")
  
}


#write out for Onitsha-Owerri
Onitsha = get_tryptich_plot("NGA", 6.913595, 5.8, "Onitsha-Owerri", year = "2020")
ggsave(Onitsha, file = "tryptich_density_plots_NGA_Onitsha-Owerri.pdf", 
       dpi = 400, width = 12, height = 10, path = "./results_plots")

Mwanza = get_tryptich_plot("TZA", 32.9175, -2.5164, "Mwanza", year = "2020")
ggsave(Mwanza, file = "tryptich_density_plots_TZA_Mwanza.pdf", 
       dpi = 400, width = 12, height = 10, path = "./results_plots")

Ndola = get_tryptich_plot("ZMB", 28.64, -12.96, "Ndola", year = "2020")
ggsave(Ndola, file = "tryptich_density_plots_ZMB_Ndola.pdf", 
       dpi = 400, width = 12, height = 10, path = "./results_plots")


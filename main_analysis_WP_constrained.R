setwd("~/Dropbox/How urban is Africa/Data")

library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(spdep)
library(Matrix)
library(exactextractr) #for fast raster extract 
library(rmapshaper)
library(igraph) # for adjacency matrix and connected components
library(countrycode)
library(spatialreg)
library(nngeo)

#read Africa shapefile 
africa = sf::st_read("gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH",
                             "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MWI",
                             "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "SWZ", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF",
                             "ZMB", "ZWE")) %>% #read African countries excluding small islands
  rmapshaper::ms_simplify(., keep = 0.1, keep_shapes = TRUE)

#create threshold, settlement, and country lists
threshold_list = as.list(c(1500, seq(200, 2000, by = 200)))
settlement_list = as.list(c(2000, 5000, 10000, 25000, 50000)) 
country_list = as.list(unique(africa$GID_0))

for (k in 1:length(country_list)) {
  
  country = africa %>%
    dplyr::filter(GID_0 == country_list[[k]])
  
  # 1) create 1x1km grid - how to get to 1km exactly? 
  country_grid = country %>%
    #use .009 to get ~1km^2 grid
    st_make_grid(cellsize = 0.009) 
  
  country_name = tolower(country$GID_0)
  
  #extract raster for country
  country_raster = raster(paste0("WorldPop/WP 2020 Constrained/", 
                                 country_name, "_ppp_2020_constrained.tif")) %>%
    raster::aggregate(., fact = 10, fun = sum, na.rm = T) %>%
    exact_extract(., country_grid, fun = "sum", force_df = T) %>%
    dplyr::rename("2020_pop_constrained" = "sum")
  
  #bind to country grid and filter only polygons that intersect the country border
  country_grid = cbind(country_raster, country_grid) %>%
    st_as_sf() %>%
    #retain only polygons that intersect given country's border ... 
    filter(st_intersects(., country, sparse = FALSE))
  
  country_pop = sum(country_grid$`2020_pop_constrained`)
  
  threshold_results = data.frame()
  for(i in 1:length(threshold_list)){
    print(paste(
      "Running ",
      "country:", country_list[[k]],
      "threshold:", threshold_list[[i]], 
      sep=' '
    )
    )
    # this entire thing should be extracted into a function.
    df = country_grid %>% 
      dplyr::filter_at(vars(1), dplyr::any_vars(. > threshold_list[[i]])) 
    print(paste("thresholded gives", nrow(df), "settlements", sep=' '))
    if(nrow(df) == 0){
      print("dataframe is empty!")
    } else if(nrow(df) == 1){
      print("dataframe finds a single settlement!")
      component_labels = c(1)
    } else {
      print("dataframe finds multiple settlements!")
      queen_graph = spdep::poly2nb(df %>% sf::as_Spatial())
      adjlist = spdep::nb2listw(queen_graph, style='B', zero.policy=TRUE)
      adj_mat = as(adjlist, "CsparseMatrix")
      graph = igraph::graph_from_adjacency_matrix(adj_mat)
      component_labels = igraph::components(graph)[[1]]
    } 
    
    if(nrow(df) > 1){
      # 4.3) get clusters and unionise geometries - work in a plot somewhere to this.
      clusters_raw = as.data.frame(component_labels) %>%
        #return geometry, population stats
        dplyr::mutate(geometry = df$geometry, 
                      ppp = df %>% st_drop_geometry %>% .[,1]) %>%
        #convert to sf
        sf::st_as_sf() %>%
        #rename variable
        dplyr::rename(cluster = `component_labels`) %>% 
        #group and unionise geometries by cluster calculating population of centres
        dplyr::group_by(cluster) %>% 
        dplyr::summarize(geometry = st_union(geometry), 
                         settlement_population = sum(ppp))
      print(clusters_raw)
      clusters = clusters_raw %>%
        #add this to fill in the holes within clusters
        nngeo::st_remove_holes()    
      #write out urban boundaries to shapefile ...  
      st_write(clusters, paste("./urban_boundaries_constrained/", 
                               country_list[[k]], "_", 
                               threshold_list[[i]], 
                               "urban_boundaries_constrained.shp", sep = ""), delete_dsn = TRUE)
      clusters = clusters %>% sf::st_drop_geometry()
      rm(df, queen_graph, adjlist, adj_mat, graph, component_labels)
      settlement_results = list()
      for(m in 1:length(settlement_list)){
        print(paste("\t with settlement threshold ", m))
        settlement = clusters %>% 
          dplyr::filter_at(vars(2), dplyr::any_vars(. > settlement_list[[m]]))
        
        #put results in some useful place ... 
        output_df = data.frame(settlement_threshold = settlement_list[[m]], 
                               n_settlements = nrow(settlement),
                               urban_population = sum(settlement$settlement_population), 
                               country_population = country_pop,
                               density_threshold = threshold_list[[i]],
                               country = country_list[[k]]
        ) %>%
          dplyr::mutate(urban_share = urban_population/country_population)
        settlement_results[[m]] = output_df
      }
    } else {
      for(m in 1:length(settlement_list)){
        settlement_results[[m]] <- data.frame(
          settlement_threshold = settlement_list[[m]],
          n_settlements = NaN,
          urban_population = NaN,
          country_population = NaN,
          density_threshold = threshold_list[[i]],
          country = country_list[[k]],
          urban_share = NaN
        )
      }
    }
    settlements = do.call("rbind", settlement_results)
    threshold_results <- rbind(threshold_results, settlements)
    
    #write out threshold results to .csv ...  
    write.csv(threshold_results, 
              file = paste(
                "./country_results_constrained/", 
                threshold_list[[i]], "_",
                country_list[[k]], 
                "_pop_grid.csv", 
                sep = ""))
    
  } 
  
}




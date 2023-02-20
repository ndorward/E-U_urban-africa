setwd("./Data")

library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(spdep)
library(Matrix)
library(exactextractr) #for fast raster extract 
library(rmapshaper)
library(foreach)
library(doParallel)
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


#### - tunable lists 
country_list = as.list(unique(africa$GID_0))

country_list = as.list(c("TCD", "TGO", "TUN", "UGA", "ZAF",
                         "ZWE"))

country_list = as.list(c("SSD", "SWZ", "GNQ"))

epoch_list = as.list(c("2000", "2010", "2020"))
threshold_list = as.list(c(1500, seq(200, 2000, by = 200)))
settlement_list = as.list(c(2000, 5000, 10000, 25000, 50000)) #add 2000

#setup cluster for parallel computing - https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
n.cores = parallel::detectCores() - 1 #define number of cores

#create cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

#register cluster
doParallel::registerDoParallel(cl = my.cluster)

#loop to run and store results over different countries, epochs, and density thresholds
ptm = proc.time()

#for every country k - for final runs put parrelisation over countries only so this script runs sequentially for simultaneously for countries
#can do 15 countries per iteration.

for (k in 1:length(country_list)) {
  
  country = africa %>%
    dplyr::filter(GID_0 == country_list[[k]])
  
  # 1) create 1x1km grid - how to get to 1km exactly? 
  country_grid = country %>%
    #use .009 to get ~1km^2 grid
    st_make_grid(cellsize = 0.009) 
  
  # 2) extract population estimates to grid using exact extract
  #read world pop
  rastlist = list.files(path = "WorldPop", 
                        pattern='.tif$', all.files=TRUE, full.names=FALSE)
  setwd("./WorldPop")
  allrasters = lapply(rastlist, raster)
  setwd("../")
  
  #extract raster to grid cells - run in loop paralellising tasks
  extract_results = foreach(l = 1:length(allrasters)) %dopar% {
    raster::crs(allrasters[[l]]) = "EPSG:4326"
    exactextractr::exact_extract(allrasters[[l]], country_grid, fun = "sum")
  }
  
  rm(allrasters)
  
  #create data frame
  pop_df = data.frame(matrix(unlist(extract_results), ncol = length(extract_results), byrow=FALSE)) %>%
    `colnames<-`(c("2000", "2010", "2020")) 
  
  rm(extract_results)
  
  #join to country grid for geometry
  country_grid = cbind(pop_df, country_grid) %>%
    st_as_sf() %>%
    #retain only polygons that intersect given country's border ... 
    filter(st_intersects(., country, sparse = FALSE))
  
  rm(pop_df)
  
  # 3) for each epoch J in country K 
  epoch_results = list()
  for(j in 1:length(epoch_list)) {
    #select epoch from list
    epoch = country_grid %>% dplyr::select(epoch_list[[j]]) 
    
    country_pop = sum(epoch[[1]])
    
    # 4) for each threshold, i, in country K and epoch J (in parallel)
    #threshold_results = foreach(i = 1:length(threshold_list)) %dopar% {
    threshold_results <- data.frame()
    for(i in 1:length(threshold_list)){
          print(paste(
            "Running ",
            "country:", country_list[[k]],
            "epoch:", epoch_list[[j]],
            "threshold:", threshold_list[[i]], 
            sep=' '
            )
          )
          # this entire thing should be extracted into a function.
              df = epoch %>% 
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
                st_write(clusters, paste("./urban_boundaries/", 
                    country_list[[k]], "_", 
                    epoch_list[[j]], "_", 
                    threshold_list[[i]], 
                    "threshold_boundaries.shp", sep = ""), delete_dsn = TRUE)
                clusters = clusters %>% sf::st_drop_geometry()
                rm(df, queen_graph, adjlist, adj_mat, graph, component_labels)
                print("epoch has values")  
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
                                       epoch = epoch_list[[j]],
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
                      epoch = epoch_list[[j]],
                      density_threshold = threshold_list[[i]],
                      country = country_list[[k]],
                      urban_share = NaN
                  )
                }
              }
            settlements = do.call("rbind", settlement_results)
            threshold_results <- rbind(threshold_results, settlements)
    } # end of doPar() or loop over settlement thresholds
    
    #run out to csv here after epoch ... 
    
    #collect epoch results from thresholds 
    epoch_results[[j]] = threshold_results
  
  }
  
  #get epoch results in useful data structure
  epoch_df = do.call("rbind", epoch_results)
  
  write.csv(epoch_df, 
    file = paste(
      "./country_results/", 
      country_list[[k]], 
      "_pop_grid.csv", 
      sep = ""))
  
  rm(country, country_grid, epoch, epoch_df, epoch_results, pop_df, threshold_results)
  
}

proc.time() - ptm

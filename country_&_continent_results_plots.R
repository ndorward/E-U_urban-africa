setwd("~/Dropbox/How urban is Africa/Data")

library(dplyr)
library(ggplot2)
library(gridExtra)

options(scipen = 999)

# read files as list and bind by rows 
results_list = list.files(path = "country_results", 
                          pattern='.csv$', all.files=TRUE, full.names=FALSE)

#issue Levi's dataframes in very different format to mine

setwd("~/Dropbox/How urban is Africa/Data/country_results")
country_df = lapply(results_list, read.csv) %>%
  do.call("rbind", .) %>%
  dplyr::mutate(settlement_threshold = as.factor(settlement_threshold)) 
setwd("~/Dropbox/How urban is Africa/Data")


#standardise y axis on all?

# 1) Create plots for case study countries

country_list = as.list(c("GHA", "MWI", "NGA", "RWA", "TZA", "ZMB"))

#create settlement count plots
settlement_count_plots = list()
for (i in 1:length(country_list)) {
  
  pp = ggplot(country_df %>% dplyr::filter(country == country_list[[i]]), 
                aes(x=density_threshold, y=n_settlements, group=settlement_threshold, colour = settlement_threshold)) +
    geom_line() +
    xlab("Population density thresholds") + ylab("Estimated settlement count") + 
    labs(title = country_list[[i]], color = "Settlement size threshold") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    facet_wrap(~ epoch) 
  
  settlement_count_plots[[i]] = pp
  
  ggsave(pp, file = paste("settlement_count_plots_",country_list[[i]],".pdf", sep = ""), 
         dpi = 400, width = 12, height = 10, 
         path = "./results_plots")
  
}

settlement_counts = grid.arrange(settlement_count_plots[[1]], settlement_count_plots[[2]], settlement_count_plots[[3]],
             settlement_count_plots[[4]], settlement_count_plots[[5]], settlement_count_plots[[6]])

ggsave(settlement_counts, file = "settlement_count_plots.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#create urban population size plots - maybe change to log urban population ... 
urban_pop_size_plots = list()

for (i in 1:length(country_list)) {
  
  pp = ggplot(country_df %>% dplyr::filter(country == country_list[[i]]), 
              aes(x=density_threshold, y=urban_population, group=settlement_threshold, 
                  colour = settlement_threshold)) +
    geom_line() +
    xlab("Population density thresholds") + ylab("Estimated urban population") + 
    labs(title = country_list[[i]], color = "Settlement size threshold") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    facet_wrap(~ epoch) 
  
  urban_pop_size_plots[[i]] = pp
  
  ggsave(pp, file = paste("settlement_pop_size_plots_",country_list[[i]],".pdf", sep = ""), 
         dpi = 400, width = 12, height = 10, 
         path = "./results_plots")
  
}

urban_size = grid.arrange(urban_pop_size_plots[[1]], urban_pop_size_plots[[2]], 
             urban_pop_size_plots[[3]], urban_pop_size_plots[[4]], urban_pop_size_plots[[5]], urban_pop_size_plots[[6]])

ggsave(urban_size, file = "urban_pop_size.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


#create urban share plots - make sure all plots 0-1 ... 
urban_share_plots = list()

for (i in 1:length(country_list)) {
  
  pp = ggplot(country_df %>% dplyr::filter(country == country_list[[i]]), 
              aes(x=density_threshold, y=urban_share, group=settlement_threshold, 
                  colour = settlement_threshold)) +
    geom_line() +
    xlab("Population density thresholds") + ylab("Urban share") + 
    labs(title = country_list[[i]], color = "Settlement size threshold") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    facet_wrap(~ epoch) 
  
  urban_share_plots[[i]] = pp
  
  ggsave(pp, file = paste("urban_share_plots_",country_list[[i]],".pdf", sep = ""), 
         dpi = 400, width = 12, height = 10, 
         path = "./results_plots")
  
}

urban_share = grid.arrange(urban_share_plots[[1]], urban_share_plots[[2]], 
             urban_share_plots[[3]], urban_share_plots[[4]], urban_share_plots[[5]], urban_share_plots[[6]])

ggsave(urban_share, file = "urban_share.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")



# 2) Create plots for all African countries -------------------------------

#create continent data 
continent_df = country_df %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(epoch, settlement_threshold, density_threshold) %>% 
  summarise(n_settlements = sum(n_settlements), 
            urban_population = sum(urban_population), 
            urban_share = mean(urban_share))

#urban shares
africa_urban_shares = ggplot(continent_df, 
            aes(x=density_threshold, y=urban_share, group=settlement_threshold, 
                colour = settlement_threshold)) +
  geom_line() +
  xlab("Population density thresholds") + ylab("Urban share") + 
  labs(title = "Urban share by population density and settlement size thresholds for Africa", color = "Settlement size threshold") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  facet_wrap(~ epoch) 

ggsave(africa_urban_shares, file = "africa_urban_shares.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#settlement counts
africa_settlement_counts = ggplot(continent_df, 
                             aes(x=density_threshold, y=n_settlements, group=settlement_threshold, 
                                 colour = settlement_threshold)) +
  geom_line() +
  xlab("Population density thresholds") + ylab("Estimated settlement count") + 
  labs(title = "Estimated settlement count by population density and settlement size thresholds for Africa", color = "Settlement size threshold") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  facet_wrap(~ epoch) 

ggsave(africa_settlement_counts, file = "africa_settlement_counts.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


#urban population
africa_urban_population = ggplot(continent_df, 
                                  aes(x=density_threshold, y=urban_population, group=settlement_threshold, 
                                      colour = settlement_threshold)) +
  geom_line() +
  xlab("Population density thresholds") + ylab("Estimated urban population") + 
  labs(title = "Estimated urban population by population density and settlement size thresholds for Africa", color = "Settlement size threshold") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  facet_wrap(~ epoch) 

ggsave(africa_urban_population, file = "africa_urban_population.pdf", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")





setwd("~/Dropbox/How urban is Africa/Data")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(tidyr)
library(scales)
library(cowplot)
library(ggpubr)


options(scipen = 999)


# 1) preliminaries --------------------------------------------------------

#read main results
main_results = read.csv("./country_results/combined_country_results.csv") %>%
  dplyr::filter(epoch == "2020") %>%
  dplyr::select(-epoch, -X) %>%
  dplyr::mutate(data_type = "Unconstrained")

# read files as list and bind by rows 
constrained_results_list = list.files(path = "country_results_constrained", 
                          pattern='.csv$', all.files=TRUE, full.names=FALSE)

setwd("~/Dropbox/How urban is Africa/Data/country_results_constrained")
country_df = lapply(constrained_results_list, read.csv) %>%
  do.call("rbind", .) %>%
  dplyr::mutate(settlement_threshold = as.factor(settlement_threshold)) %>%
  dplyr::select(-X) %>%
  dplyr::mutate(data_type = "Constrained") %>%
  rbind(., main_results) 
setwd("~/Dropbox/How urban is Africa/Data")

country_df = country_df %>%
  dplyr::distinct()

continent_df = country_df %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(data_type, settlement_threshold, density_threshold) %>% 
  summarise(n_settlements = sum(n_settlements), 
            urban_population = sum(urban_population), 
            urban_share = mean(urban_share)) %>%
  dplyr::filter(!density_threshold == 1500)


# 2) Create comparison plots for all African countries -------------------------------

#urban shares
africa_urban_shares = ggplot(continent_df %>% dplyr::filter(data_type == "Constrained") %>% 
                             dplyr::mutate(urban_share = urban_share*100), 
                             aes(x=density_threshold, y=urban_share, group=settlement_threshold, 
                                 colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                        1400, 1600, 1800, 2000)) + 
  ylab("Population share (%)") + 
#  geom_hline(yintercept = c(46,71), linetype="dashed", color = c("blue", "red")) + 
#  annotate("text", x=1000, y=c(48, 73), label=c("UN estimate", "DEGURBA estimate"), 
#           color = c("blue", "red")) +
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.text.y=element_text(size=10, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=10), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 10), 
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))

ggsave(africa_urban_shares, file = "africa_urban_shares_constrained.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#urban population
africa_urban_population = ggplot(continent_df %>% dplyr::filter(data_type == "Constrained"), 
                                 aes(x=density_threshold, y=urban_population/1000000, group=settlement_threshold, 
                                     colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                        1400, 1600, 1800, 2000)) + 
  ylab("Estimated urban population (millions)") + 
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.text.y=element_text(size=10, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=10), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 10), 
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))

#settlement counts
africa_settlement_counts = ggplot(continent_df %>% dplyr::filter(data_type == "Constrained"), 
                                  aes(x=density_threshold, y=n_settlements, group=settlement_threshold, 
                                      colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="Population density thresholds", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                                                     1400, 1600, 1800, 2000)) + 
  #scale_y_continuous(trans = log2_trans(), breaks = c(1000, 2500, 5000, 10000, 25000)) + 
  ylab("Estimated settlement count") + 
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=10, hjust = 0.5),
        axis.text.y=element_text(size=10, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=10), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 10), 
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10)) 

ggsave(africa_settlement_counts, file = "africa_settlement_counts_constrained.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


ggsave(africa_urban_population, file = "africa_urban_population_constrained.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


# combined continent plot
combined_continent_plots = plot_grid(africa_urban_shares,
                                     africa_urban_population, 
                                     africa_settlement_counts,
                                     ncol = 1)

combined_continent_plots = ggarrange(africa_urban_shares, africa_urban_population, 
                                     africa_settlement_counts,
                     labels = c("a)", "b)", "c)"),
                     ncol = 1, nrow = 3, 
                     common.legend = TRUE, legend = "bottom")

ggsave(combined_continent_plots, file = "combined_continent_plots_constrained.jpeg", 
       dpi = 400, width = 8, height = 12, 
       path = "./results_plots")


# 3) create results plots for figures 3-5 ---------------------------------

country_df = country_df %>% dplyr::filter(data_type == "Constrained")

# 3.1) figure 3 -----------------------------------------------------------

#urban share
constrained_share_plot_NGA_RWA_ZMB = ggplot(country_df %>% dplyr::filter(country %in% c("NGA", "RWA", "ZMB")), 
                                aes(x=density_threshold, y=urban_share*100, group=settlement_threshold, 
                                    colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="Population density thresholds", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                                                     1400, 1600, 1800, 2000)) + 
  ylab("Population share (%)") + 
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15)) +
  facet_wrap(~country)

ggsave(constrained_share_plot_NGA_RWA_ZMB, file = "constrained_share_plot_NGA_RWA_ZMB.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


#urban population
constrained_population_plot_NGA_RWA_ZMB = ggplot(country_df %>% dplyr::filter(country %in% c("NGA", "RWA", "ZMB")), 
                                     aes(x=density_threshold, y=urban_population/1000000, group=settlement_threshold, 
                                         colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="Population density thresholds", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                                                     1400, 1600, 1800, 2000)) +
  scale_y_continuous(trans = log2_trans(), breaks = c(0, 1, 3, 10, 30, 100)) + 
  ylab("Estimated urban population (millions)") + 
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15)) +
  facet_wrap(~country)

ggsave(constrained_population_plot_NGA_RWA_ZMB, file = "constrained_population_plot_NGA_RWA_ZMB.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#urban settlement counts
constrained_settlement_plot_NGA_RWA_ZMB = ggplot(country_df %>% dplyr::filter(country %in% c("NGA", "RWA", "ZMB")), 
                                     aes(x=density_threshold, y=n_settlements, group=settlement_threshold, 
                                         colour = settlement_threshold)) +
  geom_line() +
  scale_x_continuous(name="Population density thresholds", breaks =c(200, 400, 600, 800, 1000, 1200, 
                                                                     1400, 1600, 1800, 2000)) + 
  scale_y_continuous(trans = log2_trans(), breaks = c(0, 1, 10, 100, 1000)) + 
  ylab("Estimated settlement count") + 
  labs(title = "", color = "Settlement size threshold") +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=10, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom", 
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=15)) +
  facet_wrap(~country)

ggsave(constrained_settlement_plot_NGA_RWA_ZMB, file = "constrained_settlement_plot_NGA_RWA_ZMB.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")





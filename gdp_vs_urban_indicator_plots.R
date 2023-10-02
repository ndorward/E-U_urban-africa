setwd("~/Dropbox/How urban is Africa/Data")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(ggpmisc)

#correlation between GDP PPP 2015 & urban indicators at 400, 1200 and 2000 ppkm^2

# read files as list and bind by rows 
constrained_results_list = list.files(path = "country_results_constrained", 
                                      pattern='.csv$', all.files=TRUE, full.names=FALSE)

setwd("~/Dropbox/How urban is Africa/Data/country_results_constrained")
constrained_df = lapply(constrained_results_list, read.csv) %>%
  do.call("rbind", .) %>%
  dplyr::mutate(settlement_threshold = as.factor(settlement_threshold)) %>%
  dplyr::select(-X) %>%
  dplyr::mutate(data_type = "Constrained", 
                epoch = "2020") %>%
  dplyr::distinct()
setwd("~/Dropbox/How urban is Africa/Data")

#read gdp data 
gdp = read.csv("./WB_GDP_PPP_2015_Const_USD/WB_GDP_PPP_2015_Const_USD.csv", skip = 4) %>% 
  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
                                                 warn = FALSE)) %>%
  dplyr::select(-67) %>%
  tidyr::pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% c(2000, 2010, 2020)) %>%
  dplyr::arrange(country_iso, year)

ssd = rbind(gdp %>% dplyr::filter(country_iso == "SDN" & year %in% 1960:2010),
            gdp %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2020)) %>%
  dplyr::mutate(country_iso = "SSD")

gdp = rbind(gdp %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
  dplyr::rename(gdp = value) %>%
  dplyr::select(country_iso, year, gdp) %>%
  dplyr::mutate(epoch = as.character(year))

#join GDP data to country df and filter density thresholds
country_df_pooled = constrained_df %>%
  dplyr::rename(country_iso = country) %>%
  dplyr::left_join(., gdp %>% dplyr::select(-year), by =  c("country_iso", "epoch")) %>%
  dplyr::filter(density_threshold %in% c(400, 1200, 2000) & settlement_threshold == 50000 & data_type == "Constrained")

# 2) Plots for paper - 2020 -----------------------------------------------

#check for places zero

#urban share
gdp_vs_urban_share = ggplot(country_df_pooled, 
                            aes(x=log(gdp), y=urban_share)) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Population share") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 15),
        legend.position="bottom")

ggsave(gdp_vs_urban_share, file = "gdp_vs_urban_share_2020.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#settlement count  
gdp_vs_n_settlements = ggplot(country_df_pooled, aes(x=log(gdp), y=log(1+n_settlements))) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Estimated settlement count (log)") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~ density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20), 
        strip.text.x = element_text(size = 15),
        legend.position="bottom")

ggsave(gdp_vs_n_settlements, file = "gdp_vs_n_settlements_2020.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#urban population size
gdp_vs_urban_population = ggplot(country_df_pooled, aes(x=log(gdp), y=log(urban_population))) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Estimated urban population (log)") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~ density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20), 
        strip.text.x = element_text(size = 15),
        legend.position="bottom")

ggsave(gdp_vs_urban_population, file = "gdp_vs_urban_population_2020.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")










#pooled plots - not in paper ... 

#urban share
gdp_vs_urban_share = ggplot(country_df_pooled, aes(x=log(gdp), y=urban_share)) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Population share") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~ density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom")

ggsave(gdp_vs_urban_share, file = "gdp_vs_urban_share_pooled.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#settlement count  
gdp_vs_n_settlements = ggplot(country_df_pooled, aes(x=log(gdp), y=log(1+n_settlements))) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Estimated settlement count (log)") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~ density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom")

ggsave(gdp_vs_n_settlements, file = "gdp_vs_n_settlements_pooled.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")

#urban population size
gdp_vs_urban_population = ggplot(country_df_pooled, aes(x=log(gdp), y=log(urban_population))) + 
  geom_point() +
  geom_smooth(method=lm) + 
  geom_rug() +
  stat_poly_eq() +
  ylab("Estimated urban population (log)") + xlab("GDP per capita US$ (PPP, 2015 constant prices) (log)") + 
  facet_wrap(~ density_threshold) + 
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.text.x=element_text(size=15, color="black"),
        axis.title=element_text(size=20), 
        legend.position="bottom")

ggsave(gdp_vs_urban_population, file = "gdp_vs_urban_population_pooled.jpeg", 
       dpi = 400, width = 12, height = 10, 
       path = "./results_plots")


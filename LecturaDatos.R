install.packages("thomasp85/patchwork", dependencies = TRUE)
install.packages("getTBinR")
library(scales)
library(tibble)
library(getTBinR)
library (tidyverse)
library (janitor)
library(jcolors)
library(tidyverse)
library(rworldmap)
library(viridis)
library(hrbrthemes)
library(extrafont)
library(DT)
library(dplyr)
library(gifski) ##requires cargo on ubuntu
library(patchwork)

library(showtext) 

tuberculosis <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-22/tuberculosis_oms.csv")
View(tuberculosis)

tb_dict <- get_data_dict(verbose = FALSE)
View(tb_dict)


regional_incidence <- plot_tb_burden_summary(conf = NULL, compare_to_world = FALSE) +
  labs(title = "Tuberculosis (TB) Incidence Rates",
       subtitle = "By Region",
       caption = "") +
  facet_wrap(~Area, scales = "free_y") +
  theme(legend.position = "none", plot.title = element_text(size=22)) +
  ##gganimate code
  transition_reveal(year)
# TRABAJAR CON SERIES TEMPORALES 
# PROBAR HIGHCHART
# BUSCAR INSPIRACION EN EL MAPA

library(dplyr)

global_tb <- summarise_tb_burden(compare_to_world = TRUE,
                                 annual_change = TRUE,
                                 stat = "rate",
                                 verbose = FALSE) %>% filter(area == "Global")%>%View()
country <- "United Kingdom"
interactive <- FALSE

datos2000 <-tuberculosis%>%filter(anio>1999)%>%group_by(pais,anio)%>%
  summarise(casosTotal=sum(nuevosrecaida_h014))
  View(datos2000)

#%>%View()
plot_tb_burden_overview(countries = 'Argentina',
                        compare_to_region = TRUE,
                        interactive = interactive,
                        verbose = FALSE)

# Exploring Estimates of the Tuberculosis Case Fatality Ratio - with getTBinR
mp1 <- map_tb_burden(year = 2017, verbose = FALSE) +
  scale_fill_viridis(option = "cividis", direction = -1) + labs(title = "Map of Tuberculosis Incidence Rates - 2016",
       subtitle = "Incidence rates are per 100,000 population")

mp1 +
  labs(caption = "@seabbs Source: WHO")

tb_burden <- get_tb_burden(verbose = FALSE)
dict <- get_data_dict(verbose = FALSE)

region_case_fat <- tb_burden %>% 
  filter(year %in% 2016) %>% 
  group_by(year, g_whoregion) %>% 
  summarise(mean = mean(cfr, na.rm = TRUE),
            sd = sd(cfr, na.rm = TRUE)) %>% 
  mutate(ll = mean - sd,
         lll = mean - 2*sd,
         hh = mean + sd,
         hhh = mean + 2 * sd)

region_case_fat


highest_case_fataltity_countries <- tb_burden %>% 
  filter(year %in% 2016) %>% 
  arrange(desc(cfr)) %>% 
  slice(1:10) %>% 
  pull(country)

highest_case_fataltity_countries


plot_region_case_fatality <- region_case_fatality %>%
  plot_rate_region(metric = "case_fat_rate",
                   title = "Tuberculosis Case Fatality Rate",
                   subtitle = "By WHO region: 2000 to 2016",
                   scales = "free_y",
                   y_lab = "Estimated TB Case Fatality Ratio") +
  labs(caption = "Case fatality ratio estimated by taking the ratio of TB mortality rates and TB incidence rates each year in all years. For 2016 
       the mean regional case fatality ratio estimated by the WHO is also shown (along with one and two standard deviations)") +
  geom_point(data = region_case_fat, aes(y = mean, x = year, fill = g_whoregion), shape = 2, size = 1.3, col = "black") +
  geom_linerange(data = region_case_fat, aes(ymin = ll, ymax = hh, y = NULL), alpha = 0.4, size = 1.2, col = "black") +
  geom_linerange(data = region_case_fat, aes(ymin = lll, ymax = hhh, y = NULL), alpha = 0.2, size = 1.2, col = "black")

plot_region_case_fatality +
  labs(caption = "@seabbs Source: WHO")
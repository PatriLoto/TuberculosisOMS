install.packages("thomasp85/patchwork", dependencies = TRUE)
install.packages("getTBinR")
install.packages("hrbrthemes")
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
datos2000 <-tuberculosis%>%filter(anio>1999)%>%group_by(pais,anio)%>%
  summarise(casosTota014=sum(nuevosrecaida_h014), casosTotal1524=sum(nuevosrecaida_h1524), casosTotal2534=nuevosrecaida_h2534, casosTotal3544=sum(nuevosrecaida_h3534))
View(datos2000)


tb_dict <- get_data_dict(verbose = FALSE)
View(tb_dict)

#funciona
regional_incidence <- plot_tb_burden_summary(conf = NULL, compare_to_world = FALSE) +
  labs(title = "Tuberculosis (TB) Incidence Rates",
       subtitle = "By Region",
       caption = "") +
  facet_wrap(~Area, scales = "free_y") +
  theme(legend.position = "none", plot.title = element_text(size=22)) 
  ##gganimate code
  #transition_reveal(year)

regional_incidence

plot_tb_burden_summary


plot <- plot + scale_colour_viridis(end = viridis_end, direction = viridis_direction, 
                                    discrete = TRUE, option = viridis_palette) + scale_fill_viridis(end = viridis_end, 
                                                                                                    direction = viridis_direction, discrete = TRUE, option = viridis_palette) + 
  theme_minimal() + theme(legend.position = legend) + labs(x = "Year", 
                                                           y = metric_label, caption = "Source: World Health Organisation")
if (annual_change) {
  plot <- plot + scale_y_continuous(labels = percent, trans = trans)
}
else {
  plot <- plot + scale_y_continuous(trans = trans)
}
if (!is.null(facet)) {
  plot <- plot + facet_wrap(facet, scales = scales)
}
if (interactive) {
  plot <- ggplotly(plot) %>% style(hoverlabel = list(bgcolor = "white"), 
                                   hoveron = "fill")
}
return(plot)

# TRABAJAR CON SERIES TEMPORALES 
# PROBAR HIGHCHART
# BUSCAR INSPIRACION EN EL MAPA

library(dplyr)

global_tb <- summarise_tb_burden(compare_to_world = TRUE,
                                 annual_change = TRUE,
                                 stat = "rate",
                                 verbose = FALSE) %>% filter(area == "Global")%>%View()

#---------------------------------------------------------------------
#funciona  Twitter 1 cuadro comparativo
# introducís un país y te lo compara con el resto de los países del continete al cual pertenece
country <- "Argentina" #United Kingdom #Argentina
interactive <- FALSE

#%>%View()
plot_tb_burden_overview(countries = 'Argentina',
                        compare_to_region = TRUE,
                        interactive = interactive,
                        verbose = FALSE)+
  labs(x="",y="Incidencia estimada por 100.000 habitantes", title="Comparativo de incidencia en  Países del contienente Americano",
       caption = "Source: Organización Mundial de la Salud")


#---------------------------------------------------------------------
#function:
function (df = NULL, dict = NULL, metric = "e_inc_100k", metric_label = NULL, 
          countries = NULL, years = NULL, compare_to_region = FALSE, 
          facet = NULL, annual_change = FALSE, trans = "identity", 
          legend = "right", scales = "free_y", interactive = FALSE, 
          download_data = TRUE, save = TRUE, viridis_palette = "viridis", 
          viridis_direction = -1, viridis_end = 0.9, verbose = TRUE, 
          ...) 
{
  Year <- NULL
  df_prep <- prepare_df_plot(df = df, dict = dict, metric = metric, 
                             metric_label = metric_label, countries = countries, years = years, 
                             compare_to_region = compare_to_region, facet = facet, 
                             annual_change = annual_change, trans = trans, download_data = download_data, 
                             save = save, verbose = verbose, ...)
  country <- NULL
  plot <- ggplot(df_prep$df, aes(x = country, y = .data[[df_prep$metric_label]], 
                                 col = Year)) + geom_point(alpha = 0.6, size = 1.5, na.rm = TRUE)
  plot <- plot + scale_colour_viridis(end = viridis_end, direction = viridis_direction, 
                                      discrete = FALSE, trans = trans, option = viridis_palette) + 
    theme_minimal() + theme(legend.position = legend) + labs(x = "", 
                                                             y = df_prep$metric_label, title= "Comparativo de incidencia en  Países del contienente Americano",caption = "Source: Organización Mundial de la Salud") + 
    coord_flip()
  if (annual_change) {
    plot <- plot + scale_y_continuous(labels = percent, trans = trans) + 
      geom_hline(yintercept = 0, linetype = 2, alpha = 0.6)
  }
  else {
    plot <- plot + scale_y_continuous(trans = trans)
  }
  if (!is.null(df_prep$facet)) {
    plot <- plot + facet_wrap(df_prep$facet, scales = scales)
  }
  if (interactive) {
    plot <- plotly::ggplotly(plot) %>% style(hoverlabel = list(bgcolor = "white"), 
                                             hoveron = "fill")
  }
  return(plot)
}
plot
#funcion
#-----------------------------------------------------------------------

#Funciona twitter 2  le pasas como parámetro el año y obtenes este mapa

# Exploring Estimates of the Tuberculosis Case Fatality Ratio - with getTBinR
# Explorando estimaciones de la tasa de mortalidad de casos de tuberculosis

mp1 <- map_tb_burden(year = 2017,legend = "bottom", metric_label= "Incidencia estimada cada 100.000 habitantes", verbose = FALSE) +
  scale_fill_viridis(option = "C", direction = -1) + labs(title = "Mapa de la tasa de incidencia de la tuberculosis - 2017",   #Map of Tuberculosis Incidence Rates - 2017
                                                                subtitle = "Tasa de incidencia cada 100,000 habitantes")  #Incidence rates are per 100,000 population
#se le puede cambiar la paleta:
#"magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
mp1 +
  labs(caption= str_c(str_wrap( "A nivel mundial, la incidencia de la tuberculosis está reduciéndose a un ritmo del 2% anual aproximadamente. Esta cifra debe aumentar al 4-5% con el fin de alcanzar las metas para 2020 de la Estrategia Fin a la TB.", width = 140),
                      "\nData Source:Organización Mundial de la Salud \nVisualización: @PatriLoto")) +
  
  
  theme( axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         legend.background = element_rect(fill='grey'),
         plot.background = element_rect(fill='grey',color ='grey' ),
        panel.background =element_rect(fill='grey'),
        legend.key = element_rect(fill = "grey", color = NA),
        legend.title = element_text(color = "black", size = 8, hjust = 0.5),
        plot.title = element_text(hjust = 0.5,
                                  size=15,colour = 'black', #
                                  face = 'bold'),
        plot.caption = element_text(hjust =0.9,
                                    size=7,
                                    #face = 'bold',
                                    colour = 'grey30'))
  
  
ggsave("mapa7.17.png", width = 7.4, height = 4.5)
ggsave("mapafinal3.1.png", width = 6.5, height = 5.5)
#-------------------------------------------------------------------------------------------------
tb_burden <- get_tb_burden(verbose = FALSE)
dict <- get_data_dict(verbose = FALSE)





region_case_fat <- tb_burden %>% 
  filter(year %in% 2005) %>% 
  group_by(year, g_whoregion) %>% 
  summarise(mean = mean(cfr, na.rm = TRUE),
            sd = sd(cfr, na.rm = TRUE)) %>% 
  mutate(ll = mean - sd,
         lll = mean - 2*sd,
         hh = mean + sd,
         hhh = mean + 2 * sd)

View(region_case_fat)


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

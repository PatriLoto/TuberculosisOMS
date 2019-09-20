install.packages("getTBinR")  #paquete desarrollado por Sam Abbott(@seabbs), dicho paquete trabaja con el conjunto de 
#datasets completos sobre la tuberculosis

library(scales)
library(gganimate)
library(getTBinR)
library (tidyverse)
#library(jcolors)
library(tidyverse)
library(viridis)
library(DT)
library(gifski) 

#--------------------------------------------------------------------
#Lectura de datos de miercoles
tuberculosis <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-22/tuberculosis_oms.csv")
View(tuberculosis)
datos2000 <-tuberculosis%>%filter(anio>1999)%>%group_by(pais,anio)%>%
  summarise(casosTota014=sum(nuevosrecaida_h014), casosTotal1524=sum(nuevosrecaida_h1524), casosTotal2534=nuevosrecaida_h2534, casosTotal3544=sum(nuevosrecaida_h3534))
View(datos2000)
#-------------------------------------------------------------------
# diccionario del conjunto de datasets 
tb_dict <- get_data_dict(verbose = FALSE)
# primer vistazo de los datasets
datatable(tb_dict)
#-------------------------------------------------------------------
#Resúmen de los datos por país y por año
tabla <-summarise_tb_burden(metric = "e_inc_num",
                              stat = "rate",
                              countries = "America", 
                              compare_to_world = FALSE, 
                              compare_to_region = FALSE,
                              verbose = FALSE) 
datatable(tabla, colnames = c('Área', 'Año', 'tasa incremento', 'tasa incremento_baja', 'tasa incremento_alta'), options = list(searchHighlight = TRUE))
#-----------------------------------------------------------------
# Evolución de la Tasa de incidentes de tuberculosis por region

regional_incidence <- plot_tb_burden_summary(conf = NULL, compare_to_world = FALSE) +
      labs(title = "Evolución de la Tasa de incidencia \n de tuberculosis por Región",
          caption = "", x="", 
          y="Incidencia estimada \n cada 100.000 habs.") +
     facet_wrap(~Area, scales = "free_y") +
     theme(legend.position = "none", plot.title = element_text(size=22, hjust = 0.5)) +
  ##gganimate code
  transition_reveal(year)

regional_incidence
#----------------------------------------------------------------------------------

global_tb <- summarise_tb_burden(compare_to_world = TRUE,
                                 annual_change = TRUE,
                                 stat = "rate",
                                 verbose = FALSE) %>% filter(area == "Global")%>%View()
#-------------------------------#---------------------------------------------------------------------
#compara el valor de un país convalores de otros países
# sólo es necesario pasarle como parámetro el País, la metrica,
#si deseamos que compare con los países de la región y si queremosque el 
#gráfico sea interactivo, entonces en ambos casos le pasamos TRUE
getTBinR::plot_tb_burden_overview(metric = "e_inc_100k",
                                  countries = "Agentina",
                                  compare_to_region = TRUE,
                                  interactive = FALSE,metric_label = "Incidencia estimada cada 100.000 habitantes")
#------------------------------------------------------------------------------------------------------------------
#serie de tiempo desde 2000 a 2015
getTBinR::plot_tb_burden(metric = "e_inc_100k",
                         countries = "Argentina",
                         interactive = FALSE, metric_label = "Incidencia estimada cada 100.000 habs") +
      labs(x="",y="Incidencia estimada",  
           title="Evolución de la tasa de incidencia de la tuberculosis en Argentina",
           caption= str_c(str_wrap("** la tasa de incidencia se estima cada 100.000 habitantes", width = 130), "\n Fuente: Organización Mundial de la Salud"))+
      theme(axis.text.y = element_text(vjust = 1.5,  size=8),
        plot.title = element_text(hjust = 0.3,
                                  size=13,colour = 'black', face = 'bold'))
#-------------------------------------------------------------------------------------------------------------------
# Reporte de tuberculosis para Argentina
# debemos pasarle el país del cual queremos el informe

render_country_report(country = "Argentina", save_dir = ".")
#-------------------------------------------------------------------------------
# cuadro comparativo cdela tasa de incidencia con otros paìseps
p<-plot_tb_burden_overview(countries = 'Argentina',
                        compare_to_region = TRUE,
                        interactive = interactive,
                        verbose = FALSE)+ scale_fill_gradient() +
      labs(x="",y="Incidencia estimada cada 100.000 habitantes",  
           title="Comparativo de la tasa de incidencia \en  Países del continente Americano",
           caption = "Source: Organización Mundial de la Salud")+
      theme(axis.text.y = element_text(vjust = 1.5,  size=8),
             plot.title = element_text(hjust = 0.5,
                                  size=15,colour = 'black', #
                                  face = 'bold'),
        legend.title=element_text())
p

ggplotly
p2 <- ggplotly(text = paste(hoverformat='2.F', tooltip = "text"))

p2
#-----------------------------------------------------------------------
# graficar un mapa con las tasas de incidencia con una sola línea de código
# Función map_tb_burden: le pasas como parámetro el año y obtenes el mapa

getTBinR::map_tb_burden(metric = "e_inc_100k")

#-----------------------------------------------------------------
# Mapa publicado
# Mapa customizado

mp1 <- map_tb_burden(year = 2017,legend = "bottom", metric_label= "Tasa de incidencia cada 100,000 habitantes", verbose = FALSE) +
  scale_fill_viridis(option = "C", direction = -1) + labs(title = "Mapa de la tasa de incidencia de la tuberculosis - 2017") 
# se le puede cambiar la paleta:
# "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
mp1 +
  labs(caption= str_c(str_wrap( "A nivel mundial, la incidencia de la tuberculosis está reduciéndose 
                                a un ritmo del 2% anual aproximadamente. Esta cifra debe aumentar al 4-5% con el fin de alcanzar las metas para 2020 de la Estrategia Fin a la TB.", width = 130),
                      "\nData Source:Organización Mundial de la Salud \nVisualización: @PatriLoto")) +
  
  
  theme( axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         legend.background = element_rect(fill='grey', color = 'grey'),
         plot.background = element_rect(fill='grey', color ='grey'),
         panel.background =element_rect(fill='grey',  color = 'grey'),
         legend.key = element_rect(fill = "grey", color ='grey'),
         legend.title = element_text(color = "black", size = 10, hjust = 0.5),
         plot.title = element_text(hjust = 0.5, family = 'Garamond',
                                   size=16,colour = 'black', 
                                   face = 'bold'),
         plot.caption = element_text(hjust =0.9,
                                     size=8,
                                     face = 'bold',
                                     colour = 'grey30'),
         plot.margin = margin(0.8, 0.1, 0.5, 0.1, "cm"))


ggsave("mapatasaIncidencia.png", width = 7.4, height = 4.5)
ggsave("mapafina8.2.png", width = 6.5, height = 5.5)
#-------------------------------------------------------------------------------------------------
# App interactiva de la tuberculosis en una sola línea
getTBinR::run_tb_dashboard()

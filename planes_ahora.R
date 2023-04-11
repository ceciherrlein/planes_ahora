################################################## PLANES AHORA #################################################################
  
  rm(list= ls())
  gc()
  
  ## Instalamos las librerias a utilizar 
  
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(tidyverse)
  library(dplyr)
  library(ggrepel)
  library(sf)
  library(RColorBrewer)
  library(sp)
  library(rgdal)
  library(plotly)
  library(janitor)
  library(gganimate)
  library(animation)
  library(colorRamps)
  library(lubridate)
  library(viridis)
  #library(gifsky)
  library(reshape)
  library(rmapshaper)
  
  # Importamos el mapa de Argentina 
  
  ## El mapa se descargo de IGN  
  
  mapa <- readOGR("./data/provincia.shp") %>% 
          spTransform(CRS("+init=epsg:4326 +proj=longlat
                          +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) %>% 
          st_as_sf() %>% 
          st_crop(xmin = -80, xmax = -48,
                  ymin = -54, ymax = -22) %>%  
          select(c(nam, geometry))
  
  # Cambiamos el encoding 
  
  mapa$nam<- iconv(mapa$nam, from = "UTF-8", to="ASCII//TRANSLIT")
  
  # Cambiamos el formato de la columna nam y el nombre de tierra del fuego para poder joinear 
  
  mapa$nam<- str_to_upper(mapa$nam)
  
  mapa$nam[mapa$nam == "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR"] <- "TIERRA DEL FUEGO"
  
  mapa<- rmapshaper::ms_simplify(mapa)
  
  mapa_argentina<- as_tibble(mapa)
  
  # Importamos la data de los planes ahora 
  
  planes_ahora_operaciones<- read_excel("./data/a12_web_operaciones2.xlsx", col_names = TRUE) %>% 
                             clean_names()
  
  ### Por el momento, solo vamos a trabajar con los datos del 2021y agrupado por provincia 
  
  planes_ahora_operaciones_resumen<- planes_ahora_operaciones %>% select(mes_anio, provincia, operaciones) %>% 
                                                                         group_by(mes_anio, provincia) %>% 
                                                                        summarise(cantidad_operaciones= sum(operaciones))
  
  ## Importamos los datos de la cantidad de habitantes por provincia 
  
  provincias_hab<- read.csv2("./data/poblacion.csv", header = TRUE, sep= ";") %>% 
                   clean_names() %>% slice_tail() 
  
  # Modificamos los nombres 
  
  provincias_hab<- provincias_hab %>%  
                    dplyr::rename("CIUDAD AUTONOMA DE BUENOS AIRES" = poblacion_2,
                           "BUENOS AIRES"= poblacion_6,
                           "CATAMARCA"= poblacion_10,
                           "CORDOBA"= poblacion_14,
                           "CORRIENTES"= poblacion_18,
                           "CHACO"= poblacion_22,
                           "CHUBUT"= poblacion_26,
                           "ENTRE RIOS"= poblacion_30, 
                           "FORMOSA"= poblacion_34,
                           "JUJUY"= poblacion_38, 
                           "LA PAMPA"= poblacion_42, 
                           "LA RIOJA"= poblacion_46, 
                           "MENDOZA"= poblacion_50,
                           "MISIONES"= poblacion_54,
                           "NEUQUEN"= poblacion_58, 
                           "RIO NEGRO"= poblacion_62,
                           "SALTA"= poblacion_66,
                           "SAN JUAN"= poblacion_70, 
                           "SAN LUIS"= poblacion_74,
                           "SANTA CRUZ"= poblacion_78,
                           "SANTA FE"= poblacion_82,
                           "SANTIAGO DEL ESTERO"= poblacion_86, 
                           "TUCUMAN"=poblacion_90,
                           "TIERRA DEL FUEGO"= poblacion_94) 
  
  # Cambiamos el formato del df con pivot_wider 
  
  provincias_hab_longer<- provincias_hab %>% 
    select(-indice_tiempo, -poblacion_arg) %>% 
    pivot_longer(cols= 1:24,
                 names_to = "provincia",
                 values_to= "poblacion") 
  
  # Realizamos algunas operaciones sobre el df 
  
  planes_ahora_operaciones_completo <- planes_ahora_operaciones_resumen %>%
                                       left_join(provincias_hab_longer, by= "provincia") %>% 
                                       filter(mes_anio > '2021-01-01') %>% 
                                       mutate(operaciones_x_habitante= round(cantidad_operaciones/poblacion, 2), 
                                              mes_anio= ymd(mes_anio))  
  
  planes_ahora_operaciones_completo$decil<- ntile(planes_ahora_operaciones_completo$operaciones_x_habitante, 10)
  
  # Hacemos join con el mapa y los datos 
  
  total<- as_tibble(left_join(planes_ahora_operaciones_completo, mapa_argentina, by= c("provincia"="nam")))
  
  ## Antes de graficar el mapa creamos una paleta de color 
  
  colourCount=length(unique(total$decil))
  
  getPalette<- colorRampPalette(brewer.pal(9, "RdPu"))
  
  #################################################################################################################
  
  # Graficamos 
  
  argentina<- ggplot() +
              geom_sf(data= total, aes(fill= factor(decil), geometry= geometry, group = interaction(factor(decil), mes_anio)), color= 'white') +
              geom_sf_text(data = total, aes(label= provincia, geometry= geometry), size= 2, check_overlap = FALSE, position = "identity") +
              scale_fill_manual(values= getPalette(colourCount)) +
              theme_void()
  
  anim<- argentina + transition_time(mes_anio) + labs(title= "Planes ahora 12 en el 2021", 
                                                      subtitle= "Operaciones dividido cantidad de habitantes por mes: {format(frame_time, '%Y-%m')}",
                                                      caption= "Fuente: DatosAR, INDEC e IGN") + 
                                                      theme(plot.title = element_text(face= "bold", size= 18))
     
  
  plot(anim)           
  
  # Guardamos 
  
  anim_save("output.gif", anim)
             





                                
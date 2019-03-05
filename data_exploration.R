# Data Exploration

library(dplyr)
library(zeallot)


data <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                Longitud = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                Latitud = as.numeric(sub(",", ".", Latitud, fixed = TRUE)))


data %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=Longitud, y=Latitud))

# SE CALCULA LA PROPORCION DE VIVIENDAS POR CADA DOMINIO. ESTO PARA LAS CIUDADES
#AUTOREPRESENTADAS


# LIBRERIAS ---------------------------------------------------------------
library(readr)
library(tidyverse)
library(rio)
library(foreign)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)
library(openxlsx)
library(haven)
library(janitor)


# LECTURA BASES -----------------------------------------------------------

base_precenso <- read.csv("INSUMOS/Nacional_20221207_150133_0.csv",
                          header = TRUE, sep = ";")

precenso <- base_precenso
# ANALISIS EXPLORATORIO ---------------------------------------------------

names(base_precenso)
sum(base_precenso$n_hbt, na.rm = T)

head(base_precenso) 
str(base_precenso)


# DEPURACIÓN --------------------------------------------------------------

precenso1 <- precenso %>% select(pro, can, par, zon, sec, man,loc, n_pm, n_viv, n_hbt, c_ocup ) 
precenso1 <- precenso1 %>% 
  mutate( pro= str_pad(pro, 2, "left", pad = "0"),
          can= str_pad(can, 2, "left", pad = "0"),
          par= str_pad(par, 2, "left", pad = "0"),
          zon= str_pad(zon, 3, "left", pad = "0"),
          sec= str_pad(sec, 3, "left", pad = "0"),
          man= str_pad(man, 3, "left", pad = "0"),
          #loc= str_pad(loc, 2, "left", pad = "0"),
          id_sector= paste0(pro, can, par, zon, sec),
          id_vivienda= paste0(pro, can, par, zon, sec, n_viv),
          id_manz = paste0(pro, can, par, zon, sec, man),
          ciudad_auto = paste0(pro,can,par)
          ) %>% 
  filter(c_ocup == "Particular - Ocupada")


# CALCULO PROPORCIÓN ------------------------------------------------------

v_ciu_auto <- c("170150","090150","010150","180150","110150","130850","070150",
                "080150","230150")

prop_vivienda <- precenso1 %>% group_by(pro,ciudad_auto) %>% filter(ciudad_auto %in% v_ciu_auto)  %>% 
  summarise(viv_ciudad = n()) %>% 
  left_join (precenso1 %>% group_by(pro) %>% 
  summarise(viv_total = n()), by="pro" ) %>% mutate(prop_ciudad = viv_ciudad/viv_total,
                                                    prop_resto= 1 - prop_ciudad) 

export(prop_vivienda,"proporcion ciudades autorep.xlsx")



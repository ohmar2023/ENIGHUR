rm(list = ls())
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


# BASE DEL CENSO DE VIVIENDAS ---------------------------------------------

Base_censo <- read_sav("INSUMOS/Vivienda1.sav") %>% clean_names()%>% 
  filter(vtv %in% c(1:8),vco ==1) 

# MMM ---------------------------------------------------------------------

base <- Base_censo %>% mutate(provincia = str_pad(i01,width = 2,side = "left", pad = "0"),
                         canton = str_pad(i02,width = 2,side = "left", pad = "0"),
                         parr = str_pad(i03,width = 2,side = "left", pad = "0"),
                         id_canton = paste0(provincia,canton),
                         id_parr = paste0(id_canton,parr),
                         area = urv,
                         zona = str_pad(i04,width = 3,side = "left", pad = "0"),
                         sector = str_pad(i05,width = 3,side = "left", pad = "0"),
                         id_upm = paste0(provincia, canton, parr,zona, sector)) %>% 
  group_by(id_upm, id_canton,id_parr,zona,area) %>% 
  summarise(N=n())

selec_urbano <- base %>% 
  filter(id_parr %in% c("100150","090450","140150","160150"), area ==1) %>% 
  group_by(id_parr,area) %>% 
  sample_n(1) 


selec_rural <- base %>% 
  filter(id_parr %in% c("100150","090450","140150","160150"),zona == 999) %>% 
  group_by(id_parr,zona) %>% 
  sample_n(1)

rbind(selec_urbano,selec_rural) 

write.xlsx(rbind(selec_urbano,selec_rural), "muestra_piloto.xlsx")

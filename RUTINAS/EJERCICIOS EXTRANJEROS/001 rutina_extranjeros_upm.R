
library(readxl)
library(janitor)
library(haven)
library(rio)
library(tidyverse)
library(dplyr)

per_ext_upm <- readRDS("D:/GRUPO ENIGHUR/ENIGHUR/RUTINAS/EJERCICIOS EXTRANJEROS/per_ext_upm.rds")

per_ext_upm <- per_ext_upm %>% rename("N_viv"= "N_per")

export(per_ext_upm,"per_ext_upm.xlsx")

v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

viv_extranjeros <- per_ext_upm %>% 
  mutate(cod_6 = substr(id_upm,1,6),
         dom = ifelse(cod_6 %in% v_ciudades_auto,
                      cod_6,
                      substr(id_upm,1,2))) %>% 
  group_by(dom) %>% 
  summarise(n_upm_extr=sum(tiene_ext!=0),
            N=n_distinct(id_upm),
            media = round(n_upm_extr/N,4))

#muestra <- read_excel("RUTINAS/EJERCICIOS EXTRANJEROS/Muestra Final_ENVIADA.xlsx")

r_extranjeros <- muestra %>% left_join(viv_extranjeros,by="dom") %>% 
  mutate(viv_muestra = tam*12,
         Prop_Tam = tam*media,
         Viv_extr = ceiling(Prop_Tam*2)) %>% 
  adorn_totals("row",,,,-media,-Prop_Tam)


r_extranjeros <- r_extranjeros %>% 
  select(dom,nombre_dom,tam,viv_muestra,Viv_extr)


export(r_extranjeros,"./RUTINAS/EJERCICIOS EXTRANJEROS/RESULTADOS/r_extranjeros.xlsx")



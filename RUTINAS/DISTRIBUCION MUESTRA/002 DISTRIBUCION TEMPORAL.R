
rm(list=ls())

library(tidyverse)
library(dplyr)
library(janitor)
library(readxl)
library(openxlsx)

marco_upm <- readRDS("RUTINAS/DISTRIBUCION MUESTRA/marco_upm.rds") %>% 
  clean_names() %>% 
  mutate(dom = substr(domest,1,2),
         dom = ifelse(dom == "31","06",dom))

muestra <- readRDS("D:/OMAR LLAMBO/ENIGHUR/RUTINAS/DISTRIBUCION MUESTRA/INSUMOS/muestra.rds")

muestra <- marco_upm %>% 
  left_join(select(base_3,estrato,n = tam_distr_final),by="estrato") %>% 
  group_by(estrato) %>% 
  sample_n(unique(n)) %>% 
  select(id_upm,dom,area,estrato) %>% 
  as.data.frame() %>% 
  ungroup() 

sel = muestra %>% 
  arrange(dom, area, estrato) %>% 
  cbind(mes_1 = c(1, 10, 4, 7, 2, 11, 13, 5, 8, 3, 12, 6, 9),
        mes_2 = c(1, 7, 4, 10, 2, 8, 13, 5, 11, 3, 9, 6, 12),
        mes_3 = c(1, 8, 4, 11, 2, 9, 5, 12, 3, 10, 6, 13, 7))
        # mes_3 = c(1, 9, 4, 12, 7, 2, 10, 5, 13, 8, 3, 11, 6))


# TABLAS VALIDACIÓN - BORRADOR -------------------------------------------------

# *** Dom x Mes & Estrato x Mes ***

mes_v <- c("mes_1","mes_2","mes_3")
t_dom_mes <- NULL
t_estr_mes <- NULL

for(j in mes_v){
  
  mes <- j
  aux <- sel %>% tabyl(dom,.data[[mes]]) %>% 
    adorn_totals(c("col")) %>% 
    mutate(mes = j)
  t_dom_mes <- rbind(t_dom_mes,aux) 
  
  aux_2 <- sel %>% tabyl(estrato,.data[[mes]]) %>% 
    adorn_totals(c("col"))%>% 
    mutate(mes = j)
  t_estr_mes <- rbind(t_estr_mes,aux_2)
}

# FUNCION TABLAS VALIDACIÓN ----------------------------------------------------


for(j in mes_v){
  
  t_dom_area_mes <- NULL
  for (i in unique(sel$dom)) {
    aux <- NULL
    aux_2 <- NULL
    
    aux <- sel %>% filter(dom==i) 
    aux_2 <- aux %>% tabyl(area,.data[[mes]]) %>% 
      mutate(dom = rep(unique(aux$dom),n_distinct(aux$area))) %>% 
      relocate(dom,.after=area) %>% 
      untabyl() %>% 
      adorn_totals("col",,,,-area,-dom) 
    t_dom_area_mes <- rbind(t_dom_area_mes,aux_2)
  } 

}


#Creamos un repositorio en el que exportaremos las estimaciones de la MEDIA

wb <- createWorkbook()

addWorksheet(wb,"dom_x_mes")
addWorksheet(wb,"estr_x_mes")
addWorksheet(wb,"t_dom_area_mes")

writeData(wb,sheet = "dom_x_mes",t_dom_mes)
writeData(wb,sheet = "estr_x_mes",t_estr_mes)
writeData(wb,sheet = "t_dom_area_mes",t_dom_area_mes )

saveWorkbook(wb, "PRODUCTOS/RESULTADOS CINTA/CINTA_ENIGHUR_ACUMULADA.xlsx")







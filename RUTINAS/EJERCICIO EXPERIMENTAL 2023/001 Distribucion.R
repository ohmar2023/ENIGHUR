rm(list=ls())

library(tidyverse)
library(dplyr)
library(janitor)
library(readxl)
library(rio)

# -------------------------------------------------------------------------
# LECTURA MARCO -----------------------------------------------------------
# -------------------------------------------------------------------------

marco_upm <- readRDS("RUTINAS/DISTRIBUCION MUESTRA/INSUMOS/marco_upm.rds") %>% 
  clean_names() %>% 
  mutate(dom = substr(domest,1,2),
         dom = ifelse(dom == "31","06",dom))

# -------------------------------------------------------------------------
# LECTURA MUESTRA ---------------------------------------------------------
# -------------------------------------------------------------------------
muestra <- read_excel("RUTINAS/EJERCICIO EXPERIMENTAL 2023/INSUMOS/Muestra 20 UPM.xlsx") %>% 
  clean_names()

# --- PROPORCIONES DE VIVIENDA POR ESTRATO
base_1 <- marco_upm %>%
  group_by(dom) %>% 
  mutate(N = sum(mi)) %>%
  ungroup() %>% 
  group_by(dom,estrato) %>% 
  summarise(p = sum(mi)/unique(N))
  
 # --- MULTIPLICAMOS P POR EL TAMAÑO DE LA MUESTRA EN CADA ESTRATO
base_2 <- base_1 %>% left_join(muestra) %>% 
  mutate(tam_p =  p * tam_final)

# --- CALCULAMOS LOS REDONDEOS Y DEFINIMOS EL TAMAÑO CUIDADO MIN MUESTRAL DE 3
base_3 <- base_2 %>% 
  mutate(redondeo_1 = round(tam_p),
         redondeo_1 = ifelse(redondeo_1<=2,3,redondeo_1),
         sobra = tam_p - trunc(tam_p)) %>% 
  group_by(dom) %>% 
  mutate(tam_inter = sum(redondeo_1),
         exceso = tam_inter - tam_final,
         #dif = ifelse(redondeo_1 == 3,1, abs(sobra - 0.5))) %>% 
         dif = abs(sobra - 0.5)) %>% 
  ungroup() %>% 
         arrange(dif) %>%
  group_by(dom) %>% 
  mutate(
         f = row_number(),
         #elegido = min(dif[dif>0]),
         #elegido = ifelse(dif == min(dif[dif>0]),1,0),
         #elegido = ifelse(dif == max(dif[dif<0]),2,elegido),
         #tam_distr_final = ifelse(elegido == 1,redondeo_1-exceso,redondeo_1),
         #tam_distr_final = ifelse(elegido == 2,redondeo_1+exceso,redondeo_1)) %>%
         tam_distr_final = ifelse(exceso == 2 & (f ==1 | f==2),redondeo_1-1,redondeo_1),
         tam_distr_final = ifelse(abs(exceso) == 1  & f ==1, redondeo_1-exceso,tam_distr_final)) %>% 
  ungroup() %>% 
  filter(!is.na(tam_distr_final))

# --- CONTROL PARA NO EXCEDER NI DISMINUIR EL TAMAÑO POR ESTRATO (DIF VAR DE CONTROL)
base_3 %>% group_by(dom) %>% 
  summarise(tam_original = unique(tam_final),
            tam_final_final = sum(tam_distr_final),
            dif = tam_final_final-tam_original) %>% 
  View()


# EXPORTAREMOS EL INSUMO NECESARIO PARA SEGUIR CON LA SELECCION EN EL MARCO

ruta <- "RUTINAS/EJERCICIO EXPERIMENTAL 2023/RESULTADOS/DISTRIBUCION FINAL"
export(base_3 %>% select(estrato,nh = tam_distr_final),paste0(ruta,"/distr_ejer_experimental_enighur.rds"))

names(base_3)
names(distribucion_enighur_angel)

base_3 %>% select(dom,estrato,nh = tam_distr_final) %>% adorn_totals() %>% View()
base_3 %>% group_by(dom) %>% summarise(sum(tam_distr_final))








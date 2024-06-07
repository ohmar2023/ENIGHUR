rm(list=ls())

library(tidyverse)
library(dplyr)
library(janitor)
library(readxl)

marco_upm <- readRDS("RUTINAS/DISTRIBUCION MUESTRA/marco_upm.rds") %>% 
  clean_names() %>% 
  mutate(dom = substr(domest,1,2),
         dom = ifelse(dom == "31","06",dom))

muestra <- read_excel("RUTINAS/DISTRIBUCION MUESTRA/Muestra Final_ENVIADA.xlsx") %>% 
  clean_names()

base_1 <- marco_upm %>%
  group_by(dom) %>% 
  mutate(N = sum(mi)) %>%
  ungroup() %>% 
  group_by(dom,estrato) %>% 
  summarise(p = sum(mi)/unique(N))
  
base_2 <- base_1 %>% left_join(muestra) %>% 
  mutate(tam_p =  p * tam_final)

base_3 <- base_2 %>% 
  mutate(redondeo_1 = round(tam_p),
         sobra = tam_p - trunc(tam_p)) %>% 
  group_by(dom) %>% 
  mutate(tam_inter = sum(redondeo_1),
         exceso = tam_inter - tam_final,
         dif = sobra - 0.5,
         elegido = ifelse(dif == min(dif[dif>0]),1,0),
         elegido = ifelse(dif == max(dif[dif<0]),2,elegido),
         tam_distr_final = ifelse(elegido == 1,redondeo_1-exceso,redondeo_1), %>% 
         tam_distr_final = ifelse(elegido == 2,redondeo_1+exceso,redondeo_1)) %>%
  ungroup()

base_3 %>% group_by(dom) %>% 
  summarise(tam_original = unique(tam_final),
            tam_final_final = sum(tam_distr_final),
            dif = tam_final_final-tam_original) %>% 
  View()

base_3 %>% select(dom,nombre_dom,estrato,tam_final,tam_distr_final) %>% 
  View("distr")

distribucion_enighur_angel %>% left_join(select(base_3,estrato,tam_distr_final)) %>% 
  mutate(dif = nh - tam_distr_final) %>% View("dif")



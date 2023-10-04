rm(list = ls())
library(rio)
library(tidyverse)
library(sampling)
library(openxlsx)

      #######################################################
      ######## Repartición de la muestra enviada ############
      #######################################################

# 12/07/2021 - muestra nacional 2022 enviada por Javi
mn = readRDS("RUTINAS ENDI/2223_tamanio_upm.rds")

mn1 = mn %>% 
  select(prov, n = UPM)

# 13/07/2021 - 106999
marco = readRDS("RUTINAS ENDI/20210617_marco_upm.rds")

# Generación del marco de muestreo a nivel de conglomerados
marco_congl = marco %>%
  mutate(prov = substr(id_conglomerado, 1, 2),
         # particularidad de galápagos
    id_conglomerado = ifelse(prov != "20", substr(id_upm, 1, 10), 
                                    paste0(substr(id_upm, 1,6), substr(id_upm, 9, 12)))) %>%
  group_by(id_conglomerado, prov, estrato) %>% 
  summarise(viv = sum(viv))

# A partir de la marco de conglomerados se generará una base para conocer 
# el número de conglomerados por provincia y estrato
apoyo_a = marco_congl %>% 
  mutate(area = substr(estrato, 3,3)) %>% 
  group_by(prov, area) %>% 
  summarise(Ma = sum(viv)) %>%  
  group_by(prov) %>% 
  mutate(M = sum(Ma)) %>% 
  ungroup() %>% 
  mutate(prop = Ma/M) %>% 
  left_join(mn1, by = "prov") %>% 
  mutate(na1 = n * prop,
         # como debe ser m
         na2 = ceiling(na1/6)*6,
         na3 = floor(na1/6)*6,
         d2 = abs(na1-na2),
         d3 = abs(na1-na3),
         na4 = ifelse(d3 < d2, na3, na2),
         dif = na4 - na1) %>% 
  group_by(prov) %>% 
  mutate(n1 = sum(na4)) %>% 
  ungroup() %>% 
  mutate(control = n1%%12,
         na = ifelse(control == 6, na4+3, na4))

# 14/07/2021 - 2868
sum(apoyo_a$na4)
# 14/07/2021 - 2904
# 14/07/2021 - 2964
sum(apoyo_a$na)

# 
apoyo_e = marco_congl %>% 
  group_by(estrato, prov) %>% 
  summarise(Mh = sum(viv)) %>% 
  ungroup() %>% 
  mutate(area = substr(estrato, 3,3)) %>% 
  group_by(prov, area) %>% 
  mutate(Ma = sum(Mh)) %>% 
  ungroup() %>% 
  mutate(prop = Mh/Ma) %>% 
  left_join(select(apoyo_a, prov, area, na), by = c("prov", "area")) %>% 
  mutate(nh1 = na * prop,
         nh2 = ceiling(nh1),
         nh3 = ifelse(nh2 < 4, 4, nh2)) %>% 
  group_by(prov, area) %>% 
  mutate(na1 = sum(nh3)) %>% 
  ungroup() %>% 
  mutate(dif1 = na1 - na,
         dif2 = nh3 - nh1,
         control = ifelse(nh3 == 4, 1, 0)) %>% 
  arrange(prov, area, control, desc(dif2)) %>% 
  group_by(prov, area) %>% 
  mutate(orden = row_number()) %>% 
  ungroup() %>% 
  mutate(nh = ifelse(orden <= dif1, nh3-1, nh3),
         nh = ifelse(estrato=="0921", nh+1,
                     ifelse(estrato=="0923",nh-1,nh)))

# validación
# 13/07/2021 - 2902 - TRUE
sum(apoyo_a$na1) == sum(mn1$n)
# 14/07/2021 - 2904 - TRUE
sum(apoyo_a$na) == sum(apoyo_e$nh)

### Selección ###
muestra = marco_congl %>% 
  left_join(select(apoyo_e, estrato, nh), by = "estrato") %>% 
  arrange(estrato, desc(viv)) %>% 
  group_by(estrato) %>% 
  mutate(pik = inclusionprobabilities(viv, unique(nh)),
         sel = UPrandomsystematic(pik, eps=1e-6)) %>% 
  ungroup()

# Validación
# 13/07/2021 - 3183
# 14/07/2021 - 2904
# 14/07/2021 - 2964
sum(muestra$sel)

sel = muestra %>% 
  mutate(area = substr(estrato, 3, 3)) %>% 
  filter(sel == 1) %>% 
  arrange(prov, area, estrato) %>% 
  cbind(mes = c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12)) %>% # Cambiar a c(1,7,4,10,2,8,5,11,3,9,6,12)
  arrange(prov, estrato, mes) %>% 
  cbind(panel = c(1:3))

table(sel$panel, sel$mes, useNA = "ifany")

sel1 = sel %>% 
  select(-nh, -pik, -viv, -sel)

# Exportación interna
# saveRDS(muestra, "productos/02_muestra_upm/2223_muestra_endi.rds")
# saveRDS(sel, "productos/02_muestra_upm/2223_seleccion_endi.rds")
# Exportación externa
# write.xlsx(sel1, "Productos/Seleccion ENDI 20210716.xlsx")


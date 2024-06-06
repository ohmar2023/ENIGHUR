
library(tidyverse)
library(dplyr)
library(janitor)

marco_upm <- readRDS("D:/GRUPO ENIGHUR/ENIGHUR/RUTINAS/DISTRIBUCION MUESTRA/marco_upm.rds") %>% clean_names()
names(marco_upm)

marco_upm %>% 
  filter(pro=="01") %>% 
  mutate(aux = substr(domest,1,1)) %>% 
  filter(!aux %in% c(3,4)) %>%
  group_by(pro) %>% 
  mutate(N=sum(mi)) %>% 
  ungroup() %>% 
  group_by(pro,estrato) %>% 
  summarise(p = sum(mi)/N) %>% 
  View()
  





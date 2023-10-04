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

# MMM ---------------------------------------------------------------------

Marco <- import("INSUMOS/20211025_marco_upm.rds")
upm_dom <- Marco %>% group_by(dom) %>% summarise(n=n())

upm_ciudad_faltantes <- Marco %>% mutate(cod_6 = substr(id_conglomerado,1,6)) %>% 
  filter(cod_6 %in% c("080150","230150","130150","110150")) %>% group_by(cod_6) %>% 
  summarise(n=n())

aux <- Marco %>% mutate(cod_6 = substr(id_conglomerado,1,2)) %>% 
  group_by(cod_6) %>% 
  summarise(n=n())

# BASE GASTOS -------------------------------------------------------------

base <- read_sav("INSUMOS/10 ENIGHUR11_HOGARES_AGREGADOS.SAV") %>% clean_names()
base<-base %>% filter(!is.na(d1))

# Número de UPMs por dominio
upm_dom <- base %>% select(dom = provincia,identif_2010) %>% group_by(dom) %>% 
  summarise(n=n_distinct(identif_2010)) 
upm_dom <- rbind(upm_dom , base %>% select(dom = ciudad_auto,identif_2010) %>% group_by(dom) %>% 
  summarise(n=n_distinct(identif_2010)) )
upm_dom <- rbind(upm_dom , base %>% select(dom = area,identif_2010) %>% group_by(dom) %>% 
        summarise(n=n_distinct(identif_2010)) ) %>% filter(!is.na(dom))

# Número promedio de hogares por UPM
prom_hogares_upm <- base %>% group_by(provincia,identif_2010) %>% 
                  summarise(n=n()) %>% group_by(provincia) %>% 
                  summarise(n=mean(n)) %>% rename(dom=provincia)

prom_hogares_upm <- rbind(prom_hogares_upm, base %>% group_by(ciudad_auto,identif_2010) %>% 
                  summarise(n=n()) %>%group_by(ciudad_auto) %>% 
                  summarise(n=mean(n)) %>% rename(dom=ciudad_auto))

prom_hogares_upm <- rbind(prom_hogares_upm, base %>% group_by(area,identif_2010) %>% 
                            summarise(n=n()) %>%group_by(area) %>% 
                            summarise(n=mean(n)) %>% rename(dom=area) ) %>% 
                            filter(!is.na(dom))

# Número de viviendas efectivas

aux <- base %>% group_by(identif_2010) %>% summarise(viv_efectiva = n_distinct(vivienda))
mean(aux$viv_efectiva)

# PLAN DISEÑO -------------------------------------------------------------
dis <- base %>% as_survey_design(ids = identif_2010,
                                 strata = dominio,
                                 weights = fexp_cen2010,
                                 nest = T)
options(survey.lonely.psu = "certainty")

ind <- dis %>%  group_by(area) %>% 
  summarise(d1 = survey_var(d1, vartype=c("se","ci","cv","var"),
                             na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010))

# EXPORTAR XLSX -----------------------------------------------------------

write.xlsx(ind, "var_estimada_ciudad.xlsx")

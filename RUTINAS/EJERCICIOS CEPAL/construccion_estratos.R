library(janitor)
library(tidyverse)
library(srvyr)

base <- read_sav("INSUMOS/10 ENIGHUR11_HOGARES_AGREGADOS.SAV") %>% clean_names()

base1 <- base %>% 
  mutate(estrato = paste0(dominio, area),
         estrato = str_pad(estrato,width = 3,side = "left", pad = "0")) %>% 
  mutate(ciudad = str_pad(ciudad,width = 6,side = "left", pad = "0"),
         cabecera = substr(ciudad,5,6),
         id_cabecera = ifelse(cabecera == "50",1,0),
         estr = ifelse(area == 2 & id_cabecera == 0,1,
                        ifelse(area==1 & id_cabecera==0,2,3)),
         provincia = as.character(provincia),
         provincia = str_pad(provincia,width = 2,side = "left", pad = "0"),
         estrato2 = paste0(provincia,id_cabecera, estr))

# PLAN DISEÑO -------------------------------------------------------------

dis <- base1 %>% as_survey_design(ids = identif_2010,
                                 strata = estrato2,
                                 weights = fexp_cen2010,
                                 nest = T)
options(survey.lonely.psu = "certainty")

ind <- dis %>%  group_by(area) %>% 
  summarise(d1 = survey_mean(d10, vartype=c("se","ci","cv","var"),
                            na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) 

# #prov <- ind
# 
# dom_fin <- rbind(prov, ind)
# 
# write.xlsx(dom_fin, "deff_estr2.xlsx")
# 

                
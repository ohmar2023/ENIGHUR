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

base <- read_sav("DATA/10 ENIGHUR11_HOGARES_AGREGADOS.SAV")
#
base<-base %>% filter(!is.na(d1))

var(base$d1)

## plan
dis <- base %>% as_survey_design(ids = Identif_2010,
                                 strata = Dominio,
                                 weights = Fexp_cen2010,
                                 nest = T)
options(survey.lonely.psu = "certainty")

ind  <- dis %>%  
  summarise(d1 = survey_sd (d1, vartype=c("se","ci","cv","var"),
                             na.rm = T,deff = T),n=n(),N=sum(Fexp_cen2010))


length(unique(base$Identif_2010))
aux_1 <- base %>% group_by(Identif_2010) %>% summarise(n())
mean(aux_1$`n()`)

hist(base$Fexp_cen2010)
names(base)
d <- base %>% group_by(Ciudad_Auto) %>% summarise(Var_Encuesta=var(d1))
mean(d$d1)
var(d$d1)
svymean(base$d1)

unique(base$Provincia)
base$Provincia %>% View()

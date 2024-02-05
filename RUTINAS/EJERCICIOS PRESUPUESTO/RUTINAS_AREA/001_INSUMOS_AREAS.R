rm(list = ls())
library(openxlsx)
library(tidyverse)
library(rio)
library(foreign)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)
library(openxlsx)
library(haven)
library(reshape2)
library(janitor)
library(readxl)

#-------------------------------------------------------------------------------
# Lectura base ENIGHUR 2012 ----------------------------------------------------
#-------------------------------------------------------------------------------

base <- read_sav("INSUMOS/10 ENIGHUR11_HOGARES_AGREGADOS.SAV") %>% clean_names()
#saveRDS(base,"RUTINAS/EJERCICIOS PRESUPUESTO/RESULTADOS/base_2012_enighur.rds")
#-------------------------------------------------------------------------------
# Identificadores de las ciudades autorepresentadas ----------------------------
#-------------------------------------------------------------------------------

v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

#-------------------------------------------------------------------------------
# MMM: Numero de UPMS en la poblacion ------------------------------------------
# LEctura del marco actual
#-------------------------------------------------------------------------------

Marco <- import("INSUMOS/20211025_marco_upm.rds")

upm_area <- Marco %>% group_by(area) %>% summarise(n=n()) %>% 
  rename("dom"=area)

#-------------------------------------------------------------------------------
#------------ TOTAL DE UPMS EFECTIVAS POR DOMINIO - ENIGHUR2012 ----------------
#-------------------------------------------------------------------------------

prom_hogares_upm_area <- base %>% filter(!is.na(d1)) %>% 
  group_by(area,identif_2010) %>% 
  summarise(n=n()) %>% group_by(area) %>% 
  summarise(n=mean(n)) %>% rename(dom=area) 

# -----------------------------------------------------------------------------
# Creamos la variiable estrato para poder identificar la ciudad auto, resto urb
# y resto rural
#-------------------------------------------------------------------------------

base_estrato <- base %>% mutate(id_6 = substr(identif_2010,1,6),
                                estrato = ifelse(area==1 ,1,0),
                                estrato = ifelse(area==2 ,2,estrato),
                                estrato = ifelse(id_6 %in% v_ciudades_auto & area == 1,3,estrato),
                                estrato_f = paste0(substr(identif_2010,1,2),estrato)
)

#-------------------------------------------------------------------------------
#PLAN DISEÑO: Calculo de las estimaciones --------------------------------------
#-------------------------------------------------------------------------------

dis <- base_estrato %>% as_survey_design(ids = identif_2010,
                                         strata = estrato_f,
                                         weights = fexp_cen2010,
                                         nest = T)
options(survey.lonely.psu = "adjust")

# ------------------------------------------------------------------------------
# Enlistamos las variables de diseño posibles ----------------------------------
#-------------------------------------------------------------------------------

lista_variables <- paste0("d",c(1:12))

#Creamos un repositorio en el que exportaremos las estimaciones de la MEDIA

wb <- createWorkbook()
addWorksheet(wb,"d1")
addWorksheet(wb,"d2")
addWorksheet(wb,"d3")
addWorksheet(wb,"d4")
addWorksheet(wb,"d5")
addWorksheet(wb,"d6")
addWorksheet(wb,"d7")
addWorksheet(wb,"d8")
addWorksheet(wb,"d9")
addWorksheet(wb,"d10")
addWorksheet(wb,"d11")
addWorksheet(wb,"d12")

for (i in c(12:1)){
  #-------------------------------------------------------------------------------
  #---------------------- MEDIA DE LA VARIABLE -----------------------------
  #----------------------------------------------------------------------------
  ind_area_mean <- dis %>%  group_by(area) %>% 
    summarise(d1 = survey_mean(.data[[lista_variables[i]]], 
                               vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=area)
  
  #-------------------------------------------------------------------------------
  #---------------------- DESVIACION DE LA VARIABLE -----------------------------
  #----------------------------------------------------------------------------
  ind_area_sd <- dis %>%  group_by(area) %>% 
    summarise(d1 = survey_sd(.data[[lista_variables[i]]], 
                             vartype=c("se","ci","cv","var"),
                             na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=area) 
  
  #-----------------------------------------------------------------------------
  #---------------------- CONSOLIDANDO ESTIMADORES -----------------------------
  #-----------------------------------------------------------------------------
  
  ind <- ind_area_mean %>% left_join(select(ind_area_sd,dominio,"sd"=d1),
                                     by="dominio")
  
  #-------------------------------------------------------------------------------
  #------------------ CREANDO VARIABLES FALTANTES ----------------------------
  #----------------------------------------------------------------------------
  
  ind$mer <-  (ind$d1_se/ind$d1)*1.96
  ind$rho <- (ind$d1_deff-1)/(prom_hogares_upm_area$n-1)
  ind$conf <- 0.95
  ind$tnr <- 0.2
  ind$b <- 1
  ind$upm_pobl <- upm_area$n
  ind$upm_efect <- ind$n
  ind$prom_hogares_upm <- prom_hogares_upm_area$n
  
  #-----------------------------------------------------------------------------
  #------- SELECCION DE VARIABLES PARA LA COSNTRUCCION DEL TAM -----------------
  #-----------------------------------------------------------------------------
  
  ind <- ind %>% select(dominio,d1,sd,d1_se,mer,d1_deff,N,upm_pobl,upm_efect,
                        prom_hogares_upm,b,tnr,conf,rho)
  
  
  #-----------------------------------------------------------------------------
  #---------------------- EXPORTANDO LIBRO -------------------------------------
  #-----------------------------------------------------------------------------
  writeData(wb,sheet = lista_variables[i],ind)
}

ruta <-"RUTINAS/EJERCICIOS PRESUPUESTO/INSUMOS_002/INSUMOS_TAMANIO/AREA/ESTIMACIONES_12_VARIABLES_EJER_PRESUPUESTO_AREA.xlsx"
saveWorkbook(wb, ruta)

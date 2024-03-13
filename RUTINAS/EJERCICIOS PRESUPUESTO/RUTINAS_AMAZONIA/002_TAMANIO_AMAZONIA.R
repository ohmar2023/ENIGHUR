rm(list = ls())
library(samplesize4surveys)
library(readxl)
library(janitor)
library(haven)
library(rio)

#-------------------------------------------------------------------------------
# LECTURA DE INSUMOS -----------------------------------------------------------
#-------------------------------------------------------------------------------

ruta <-"RUTINAS/EJERCICIOS PRESUPUESTO/INSUMOS_002/INSUMOS_TAMANIO/AREA/ESTIMACIONES_12_VARIABLES_EJER_PRESUPUESTO_AREA.xlsx"
d1 <- read_excel(ruta, sheet = "d1_mod")

#-------------------------------------------------------------------------------
# CALCULO TAMAÑOS --------------------------------------------------------------
#-------------------------------------------------------------------------------

base_insumos <- d1

N=base_insumos$N
M=base_insumos$upm_pobl
rho=base_insumos$rho
mu=base_insumos$d1
sigma=base_insumos$sd
delta=base_insumos$mer
conf = base_insumos$conf
m=c(11:11)

for (i in 1:dim(base_insumos)[1]){
  n <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n$HouseholdsInSample <- ceiling(n$HouseholdsInSample*(1/(1-0.2)))
  n$PSUinSample <-  ceiling(n$HouseholdsInSample/11)
  n$nombre_dom <- base_insumos$dominio[i]
  if (i ==1){
    muestra1 <- n
  }else{
    muestra1 <- rbind(muestra1,n)
  }
}
muestra1 <- muestra1 %>% relocate(nombre_dom,.before = HouseholdsPerPSU) %>% 
  as.data.frame()

tamanio_area <- muestra1 
ruta <-"RUTINAS/EJERCICIOS PRESUPUESTO/RESULTADOS/Muestra_Area.xlsx"
export(tamanio_area, ruta)


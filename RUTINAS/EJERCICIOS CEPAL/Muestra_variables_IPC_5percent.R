library(samplesize4surveys)
library(readxl)
library(janitor)
library(haven)

# LECTURA DE INSUMOS ------------------------------------------------------

d1 <- read_excel("PRODUCTOS/NUESTROS/d1_mod.xlsx", 
                           sheet = "d1_mod")


insumos <- list(d1)

lista_variables <- paste0("d",c(1:1))


# Cálculo de TNR ----------------------------------------------------------

base_cobertura <- read_sav("INSUMOS/01 ENIGHUR11_IDENTIFICACION.SAV") %>% clean_names()





# CALCULO TAMANIOS --------------------------------------------------------

# MUESTRA INSUMOS MER ENIGHUR 2012 ----------------------------------------


for (j in c(1:1)) {
  base_insumos <- insumos[[j]]
  N=base_insumos$N
  M=base_insumos$upm_pobl
  rho=base_insumos$rho
  mu=base_insumos$d1
  sigma=base_insumos$sd
  delta=base_insumos$mer*1.07
  conf = base_insumos$conf
  m=c(11:11)
  tnr=0
  
  for (i in 1:dim(base_insumos)[1]){
    n <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
    n$HouseholdsInSample <- ceiling(n$HouseholdsInSample*(1/(1-tnr)))
    n$PSUinSample <-  ceiling(n$HouseholdsInSample/11)
    n$nom_dominio <- base_insumos$nombre_dom[i]
    if (i ==1){
      muestra <- n
    }else{
      muestra <- rbind(muestra,n)
    }
  }
  muestra <- muestra %>% relocate(nom_dominio,.before = HouseholdsPerPSU)
  #writeData(wb2,sheet = lista_variables[j] ,muestra)
  
}
export(muestra,"MUESTRA.xlsx",overwrite = T)
library(rio)

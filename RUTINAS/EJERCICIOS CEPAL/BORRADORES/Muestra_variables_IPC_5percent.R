library(samplesize4surveys)
library(readxl)
library(janitor)
library(haven)
library(rio)

# LECTURA DE INSUMOS ------------------------------------------------------

d1 <- read_excel("PRODUCTOS/NUESTROS/d1_mod.xlsx", 
                           sheet = "d1_mod")


# Cálculo de TNR ----------------------------------------------------------

#base_cobertura <- read_sav("INSUMOS/01 ENIGHUR11_IDENTIFICACION.SAV") %>% clean_names()


# CALCULO TAMANIOS --------------------------------------------------------

# MUESTRA INSUMOS MER ENIGHUR 2012 ----------------------------------------


base_insumos <- d1
N=base_insumos$N
M=base_insumos$upm_pobl
rho=base_insumos$rho
mu=base_insumos$d1
sigma=base_insumos$sd
delta=base_insumos %>% mutate(mer = ifelse(id_mer == 1,mer*1.197,mer*1.1945)) %>% select(mer)
delta = delta$mer
conf = base_insumos$conf
m=c(11:11)
#tnr=0

for (i in 1:dim(base_insumos)[1]){
  n <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n$HouseholdsInSample <- ceiling(n$HouseholdsInSample*(1/(1-0.2)))
  n$PSUinSample <-  ceiling(n$HouseholdsInSample/11)
  n$nom_dominio <- base_insumos$nombre_dom[i]
  if (i ==1){
    muestra1 <- n
  }else{
    muestra1 <- rbind(muestra1,n)
  }
}
muestra1 <- muestra1 %>% relocate(nom_dominio,.before = HouseholdsPerPSU)
#writeData(wb2,sheet = lista_variables[j] ,muestra)


export(muestra1,"MUESTRA_llegar.xlsx",overwrite = T)



library(samplesize4surveys)
library(readxl)
library(janitor)
library(rio)

# LECTURA INSUMOS ---------------------------------------------------------

base_insumos <- read_excel("INSUMOS-ENIGHUR.xlsx",
                           sheet = "INSUMOS") %>% 
  clean_names() 
#base_insumos <- base_insumos[4:36,]
# mer <- c(0.01246266, 0.01413330, 0.02284811,
#          0.07432,0.09463, 0.07724,0.0486,0.06957,0.07711,0.059,0.081,0.0511,
#          0.05304,0.10122,0.063,0.08236,0.13022,0.058,0.08301,0.077,0.11356,0.09331,
#          0.05155,0.07888,0.073, 0.07,0.04655,0.04449,0.03114,0.04212,0.04518,0.065,
#          0.053,0.05353,0.03475,0.046)


# Muestra ejercicio bien hecho :D -----------------------------------------}

# MUESTRA INSUMOS MER ENIGHUR 2012 ----------------------------------------
attach(base_insumos)
N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer
conf = 0.95
m=c(11:11)
#i=17
for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  n_nacional$muestra <- base_insumos$muestra[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}

muestra_mer_12 <- muestra %>% select(dominio,PSUinSample)


# MUESTRA RANGO 3 GRUPOS --------------------------------------------------

attach(base_insumos)
N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer_n
conf = 0.95
m=c(11:11)
#i=17
for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  n_nacional$muestra <- base_insumos$muestra[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}

muestra_rango_3gr <- muestra %>% select(dominio,PSUinSample)


# MUESTRA MER mediante tamaño de ANDRES ----------------------------------------
attach(base_insumos)
N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer_nuevo
conf = 0.95
m=c(11:11)
#i=17
for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  n_nacional$muestra <- base_insumos$muestra[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}

muestra_mer_nuevo <- muestra %>% select(dominio,PSUinSample)


# MUESTRA rango 4 grupos JAVI  --------------------------------------------------
attach(base_insumos)
N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer_n_2
conf = 0.95
m=c(11:11)
#i=17
for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  n_nacional$muestra <- base_insumos$muestra[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}

muestra_4gr_javi <- muestra %>% select(dominio,PSUinSample)



# MATCH RESULTADOS --------------------------------------------------------

r_muestra <- muestra_mer_nuevo %>% left_join(muestra_mer_12,by="dominio") %>% 
              left_join(muestra_rango_3gr,by="dominio") %>% 
              left_join(muestra_4gr_javi,by="dominio")
names(r_muestra) <- c("dominio","n_predefinida","n_insumos_2012","n_3_grupos","n_4_grupos")



export(r_muestra,"PRODUCTOS/muestras_resultantes.xlsx")



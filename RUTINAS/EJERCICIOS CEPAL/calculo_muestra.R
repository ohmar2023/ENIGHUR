library(samplesize4surveys)
library(readxl)
library(janitor)

# LECTURA INSUMOS ---------------------------------------------------------

base_insumos <- read_excel("ESTRUCTURA CEPAL REUNION 002.xlsx", 
                                           sheet = "INSUMOS") %>% 
  clean_names() 
base_insumos <- base_insumos[4:36,]
# mer <- c(0.01246266, 0.01413330, 0.02284811,
#          0.07432,0.09463, 0.07724,0.0486,0.06957,0.07711,0.059,0.081,0.0511,
#          0.05304,0.10122,0.063,0.08236,0.13022,0.058,0.08301,0.077,0.11356,0.09331,
#          0.05155,0.07888,0.073, 0.07,0.04655,0.04449,0.03114,0.04212,0.04518,0.065,
#          0.053,0.05353,0.03475,0.046)


# Muestra ejercicio bien hecho :D -----------------------------------------
attach(base_insumos)

N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer
conf = 0.95
m=c(11:11)

for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  n_nacional$n_and <- base_insumos$n_an[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}
sum(muestra$PSUinSample)
muestra <- muestra %>% select(dominio,PSUinSample)
export(muestra, "PRODUCTOS/muestra ejercicio.xlsx")
rm(list = ls())

# Muestra con mer para los 3 grupos ---------------------------------------
base_insumos <- read_excel("ESTRUCTURA CEPAL REUNION 002.xlsx", 
                           sheet = "INSUMOS") %>% 
  clean_names() 

ciudades_autorepresentadas <- c("Quito", "Guayaquil", "Cuenca", "Machala", "Ambato", "Esmeraldas Ciudad", 
                                "Santo Domingo", "Manta", "Loja Ciudad")

resto <- c("Azuay", "Bolívar", "Cañar", "Carchi", "Cotopaxi", "Chimborazo", 
           "El Oro", "Esmeraldas", "Guayas", "Imbabura", "Loja", "Los Rios", 
           "Manabí", "Pichincha", "Tungurahua", "Galápagos", "Santo Domingo de los Tsachilas", 
           "Santa Elena")

amazonia <- c("Morona Santiago", "Napo", "Pastaza", "Zamora Chinchipe", "Sucumbíos", 
              "Orellana")
base_insumos <- base_insumos[4:36,] %>% mutate(mer = ifelse(dominio %in% ciudades_autorepresentadas,0.0433,
                                                     ifelse(dominio%in%resto,0.0433*1.5,0.0433*3)))


attach(base_insumos)

N=poblacion
M=upms_poblacion
rho=rho
mu=estimacion
sigma=sy_gastos
delta=mer
conf = 0.95
m=c(11:11)

for (i in 1:dim(base_insumos)[1]){
  n_nacional <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n_nacional
  n_nacional$dominio <- base_insumos$dominio[i]
  if (i ==1){
    muestra <- n_nacional
  }else{
    muestra <- rbind(muestra,n_nacional)
  }
}
sum(muestra$PSUinSample)
muestra <- muestra %>% select(dominio,PSUinSample)
export(muestra, "PRODUCTOS/muestra ejercicio con tipologias 3 grupos.xlsx")
rm(list = ls())


# Muestra Andres ----------------------------------------------------------
base_insumos <- read_excel("ESTRUCTURA CEPAL REUNION 002.xlsx", 
                           sheet = "INSUMOS") %>% 
  clean_names() 
base_insumos <- base_insumos[4:36,] %>% select(dominio, muestra_andres = n_an)
export(base_insumos, "PRODUCTOS/muestra andres.xlsx")

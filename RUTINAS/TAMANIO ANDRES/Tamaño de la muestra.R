# *============================================================================*
# *Cálculo del tamaño de muestra de la Encuesta Nacional de Ingresos y Gastos
# *============================================================================*
# *                 INSTITUTO NACIONAL DE ESTADÌSTICA Y CENSOS
# *
# *             Dirección de Infraestructura Estadística y Muestreo
# *
# *                                   DINEM
# *============================================================================*
# * Elaborado por: Andrés Albán Fernández
# *============================================================================*

#_______________________________________________________________________________
#               Directorio de trabajo y librerías a utilizar
#_______________________________________________________________________________

# Librerías a utilizar:
library(readxl)
library(writexl)

rm(list = ls())

# Especificar el directorio de trabajo donde se encuentren los archivos:
#  -  'Data.xlsx'

#setwd("/Users/andresalban/Desktop/Aleph/ENIGHUR")

#_______________________________________________________________________________
# El objetivo principal de la ENIGHUR es obtener información estadística sobre 
# los ingresos y gastos de los hogares del Ecuador.         
# Es por esto que para determinar el tamaño de muestra requerida se ha utilizado
# la informaciòn que se refiere a las estimaciones de los valores promedio de los
# componentes de ingresos y gastos reportados por la ENIGHUR 2011
#_______________________________________________________________________________

#enighur <- data.frame(read_xlsx("Data.xlsx", sheet = 1))
enighur <- data.frame(read_excel("RUTINAS/TAMANIO ANDRES/Data.xlsx"))

# Z de nivel de confianza al 95%
z <- 1.96

# Tasa de no respuesta esperada
TNR <- 0.2

# Función para calcular el tamaño de muestra
calcularN <- function(data, provincia, tipo) {
  col_e <- paste0("E_", tipo)
  col_sd <- paste0("SD_", tipo)
  
  valor_e <- data[data$dominio == provincia, col_e]
  valor_sd <- data[data$dominio == provincia, col_sd]
  r <- data[data$dominio == provincia, "r"]
  DEFF <- data[data$dominio == provincia, "deff"]
  
  if (length(valor_e) == 0 || length(valor_sd) == 0) {
    return(NULL)  # Manejo de valores faltantes
  }
  
  numerador <- z*z*valor_sd*valor_sd*DEFF
  denominador <- r*r*valor_e*valor_e*(1-TNR)  
  
  N <- ceiling(numerador/denominador)
  
  return(N)
}

# Identificación de variales
n_col <- data.frame(colnames(enighur))
n_col <- data.frame(n_col[-c(1, nrow(n_col)-1, nrow(n_col)-2, nrow(n_col)-3, nrow(n_col)-4, nrow(n_col)), ])
for (i in 1:nrow(n_col)) {
  n_col[i,1] <- gsub("^E_", "", n_col[i,1])
  n_col[i,1] <- gsub("^SD_", "", n_col[i,1])
}
n_col <- data.frame(unique(n_col[,1]))

# Cálculo de muestra para cada variable de diseño
muestra <- data.frame()
for (i in enighur[,1]) {
  for (j in n_col[,1]) {
    muestra[which(enighur[,1] == i),which(n_col[,1] == j)] <- calcularN(enighur, i, j)
  }
}
colnames(muestra) <- n_col[,1] 

# Determinación del tamaño mayor para cada dominio
N_max <- data.frame(apply(muestra, 1, max))
nom_N_max <- data.frame(colnames(muestra)[max.col(muestra, "first")])

# Agregación de variables de contexto
muestra$dominio <- enighur[1]
muestra$N_max <- N_max
muestra$var <- nom_N_max
muestra$dom <- enighur$dom
muestra$pob_u <- enighur$pob_u
muestra$pob_r <- enighur$pob_r
muestra$pob_t <- muestra$pob_u + muestra$pob_r

# Distribución de muestra por área
muestra$N_u <- ceiling((muestra$pob_u / muestra$pob_t)*muestra$N_max)
muestra$N_r <- ceiling((muestra$pob_r / muestra$pob_t)*muestra$N_max)
for (k in 1:nrow(muestra)) {
  if (muestra$dom[k] == "prov_p") {
    muestra$N_u[k,1] <- ceiling(((muestra$pob_u[k] + muestra$pob_u[k+1])/ 
                                 (muestra$pob_u[k] + muestra$pob_u[k+1] + muestra$pob_r[k]))*muestra$N_max[k,1])
    pK <- muestra$pob_u[k]/(muestra$pob_u[k]+muestra$pob_u[k+1])
    muestra$N_u[k,1] <- ceiling(muestra$N_u[k,1]*pK)
    muestra$N_r[k,1] <- ceiling((muestra$pob_r[k] / 
                                 (muestra$pob_u[k] + muestra$pob_u[k+1] + muestra$pob_r[k]))*muestra$N_max[k,1])
  }
}

# Ajuste de muestra a 12 viviendas por UPM
muestra$N_u <- ceiling(muestra$N_u/12)*12
muestra$N_r <- ceiling(muestra$N_r/12)*12
muestra$N_t <- muestra$N_u + muestra$N_r

# Tamaño de muestra final por dominio
tam_muestra <- data.frame(c(muestra$dominio,muestra$N_u,muestra$N_r,muestra$N_t))
write_xlsx(tam_muestra,"muestra.xlsx")


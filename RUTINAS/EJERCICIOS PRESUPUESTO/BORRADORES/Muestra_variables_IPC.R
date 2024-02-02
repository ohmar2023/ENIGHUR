library(samplesize4surveys)
library(readxl)


# LECTURA DE INSUMOS ------------------------------------------------------

d1 <- read_excel("INSUMOS_12.xlsx", sheet = "d1")
d2 <- read_excel("INSUMOS_12.xlsx", sheet = "d2")
d3 <- read_excel("INSUMOS_12.xlsx", sheet = "d3")
d4 <- read_excel("INSUMOS_12.xlsx", sheet = "d4")
d5 <- read_excel("INSUMOS_12.xlsx", sheet = "d5")
d6 <- read_excel("INSUMOS_12.xlsx", sheet = "d6")
d7 <- read_excel("INSUMOS_12.xlsx", sheet = "d7")
d8 <- read_excel("INSUMOS_12.xlsx", sheet = "d8")
d9 <- read_excel("INSUMOS_12.xlsx", sheet = "d9")
d10 <- read_excel("INSUMOS_12.xlsx", sheet = "d10")
d11 <- read_excel("INSUMOS_12.xlsx", sheet = "d11")
d12 <- read_excel("INSUMOS_12.xlsx", sheet = "d12")

insumos <- list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)

wb2 <- createWorkbook()
addWorksheet(wb2,"d1")
addWorksheet(wb2,"d2")
addWorksheet(wb2,"d3")
addWorksheet(wb2,"d4")
addWorksheet(wb2,"d5")
addWorksheet(wb2,"d6")
addWorksheet(wb2,"d7")
addWorksheet(wb2,"d8")
addWorksheet(wb2,"d9")
addWorksheet(wb2,"d10")
addWorksheet(wb2,"d11")
addWorksheet(wb2,"d12")

lista_variables <- paste0("d",c(1:12))
# CALCULO TAMANIOS --------------------------------------------------------

# MUESTRA INSUMOS MER ENIGHUR 2012 ----------------------------------------


for (j in c(1:12)) {
  base_insumos <- insumos[[j]]
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
    n$nom_dominio <- base_insumos$nombre_dom[i]
    if (i ==1){
      muestra <- n
    }else{
      muestra <- rbind(muestra,n)
    }
  }
  muestra <- muestra %>% relocate(nom_dominio,.before = HouseholdsPerPSU)
  writeData(wb2,sheet = lista_variables[j] ,muestra)
  
}
saveWorkbook(wb2,"MUESTRA.xlsx",overwrite = T)

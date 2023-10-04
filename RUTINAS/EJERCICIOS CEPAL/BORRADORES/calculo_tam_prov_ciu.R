#rm(list = ls())
# -------------------------------------------------------------------------
# LECTURA DE BASES DE DATOS -----------------------------------------------
# -------------------------------------------------------------------------

viviendas_por_estrato <- read_excel("PRODUCTOS/DISTRIBUCION/viviendas por estrato.xlsx")
muestra <- read_excel("PRODUCTOS/NUESTROS/muestra_.xlsx")

# -------------------------------------------------------------------------
# CREAMOS EL ID PARA EMPATAR LOS DOMINIOS CON LA BASE DE VIVIENDAS --------
# -------------------------------------------------------------------------

muestra$id_dom <- str_pad(as.character(c(1:33)),width = 2,side = "left", pad = "0")
viviendas_por_estrato <- viviendas_por_estrato %>% rename("id_dom"=provincia)
base <- viviendas_por_estrato

# -------------------------------------------------------------------------
# ORREMOS LA FUNCION PARA LOS DOM 1:24
# -------------------------------------------------------------------------

provincias_id <- as.character(str_pad(as.character(c(1:24)),width = 2,side = "left", pad = "0"))

r <- NULL
r <- distribucion_prov_ciu(viviendas_por_estrato,muestra,provincias_id[1])

for(i in c(2:24))
{
  
  a <- distribucion_prov_ciu(viviendas_por_estrato,muestra,provincias_id[i])
  r = rbind(r,a) %>% arrange(Id_Dom)
  
}

# -------------------------------------------------------------------------
# nombres de los dom y siudades auto
# -------------------------------------------------------------------------

r$dominio <-  c("Azuay",
                "Bolívar",
                "Cañar",
                "Carchi",
                "Cotopaxi",
                "Chimborazo",
                "El Oro",
                "Esmeraldas",
                "Guayas",
                "Imbabura",
                "Loja",
                "Los Rios",
                "Manabí",
                "Morona Santiago",
                "Napo",
                "Pastaza",
                "Pichincha",
                "Tungurahua",
                "Zamora Chinchipe",
                "Galápagos",
                "Sucumbíos",
                "Orellana",
                "Santo Domingo de los Tsachilas",
                "Santa Elena",
                "Quito",
                "Guayaquil",
                "Cuenca",
                "Machala",
                "Ambato",
                "Esmeraldas Ciudad",
                "Loja Ciudad",
                "Manta",
                "Santo Domingo"
                )
r <- r %>% select(Id_Dom, dominio, Tam_final)
r %>% adorn_totals() %>% View()

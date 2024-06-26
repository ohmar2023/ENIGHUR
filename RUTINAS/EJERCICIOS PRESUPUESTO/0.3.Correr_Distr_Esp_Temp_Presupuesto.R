# ------------------------------------------------------------------------------
# Este script corre el csript 0.5. Realiza la distribucion y exporta un xlsx.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# PROV + CIUD
# ------------------------------------------------------------------------------

#pichincha 17+25
#guayas 09+26
#azuay 01+27
#eloro 07+28
#tungurahua 18+29
#esmeraldas 08+30
#loja 11+31
#manabi 13+32
#santo domingo 23+33

# ------------------------------------------------------------------------------
# LEER MUESTRA FINAL ENVIADA
# ------------------------------------------------------------------------------

m_final <- read.xlsx("Muestra Final_ENVIADA.xlsx") 

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------

db1 <- m_final %>% mutate(Tam_final = ceiling(Tam_final + (1/6)*Tam_final)) %>% 
  adorn_totals()

# ------------------------------------------------------------------------------
# ARMAR ZONALES
# ------------------------------------------------------------------------------

z_norte <- c("04","08","10","17","21","25","30")
z_centro <- c("02","05","06","15","16","18","22","29")
z_litoral <- c("09","12","13","23","24","26","32","33")
z_sur <- c("01","03","11","07","14","19","27","28","31")

db1 <- db1 %>% mutate(Zonal = case_when( Id_Dom %in% z_norte ~ "Norte",
                                         Id_Dom %in% z_centro ~ "Centro",
                                         Id_Dom %in% z_litoral ~ "Litoral",
                                         Id_Dom %in% z_sur ~ "Sur"))

# ------------------------------------------------------------------------------
# MULTIPLOS DE 3
# SE CONSIDERAN MULTIPLOS DE 3 SOLO PARA EL LITORAL Y CENTRO
# ------------------------------------------------------------------------------

# db1 <- db1 %>% mutate(aux0 = if_else(Zonal=="Litoral"| Zonal=="Centro",1,0),
#                       aux1 = Tam_final%%3,
#                       aux2 = if_else(aux1==2,2,0),
#                       aux3 = if_else(aux1==1,1,0),
#                       Tamanio2 = if_else(aux0==1,-aux2-aux3+Tam_final,Tam_final)) 
# db1 %>% group_by(Zonal) %>% summarise(sum(Tamanio2))

db1$Tamanio2 <- db1$Tam_final

# ------------------------------------------------------------------------------
# ARMANDO 24 DOMINIOS: A Tungurahua se le quita una UPM para obtener un número
# entero de encuestadores. Esto no afecta en nada.
# ------------------------------------------------------------------------------

db2 <- db1[-dim(db1)[1],]
aux1 <- c("17","09","01","07","18","08","11","13","23")
aux2 <- c("25","26","27","28","29","30","31","32","33")
db2[db2$Id_Dom %in% aux2,]$nombre_dom <- c("Pichincha","Guayas","Azuay","El Oro",
                                           "Tungurahua","Esmeraldas","Loja",
                                           "Manabí",
                                           "Santo Domingo de los Tsachilas"
)
db2 <- db2 %>% group_by(nombre_dom) %>% summarise(Tamanio = sum(Tamanio2))  
db2 <- db2 %>% left_join(select(db1,Id_Dom,Zonal,nombre_dom),by="nombre_dom") %>% 
  select(Id_Dom,dominio = nombre_dom,Tamanio,Zonal) %>% arrange(Id_Dom)
db2 <- db2[!db2$dominio=="Galápagos",]
db2[db2$dominio=="Tungurahua",3] <- db2[db2$dominio=="Tungurahua",3]-1

# ------------------------------------------------------------------------------
# USANDO FUNCIONES
# ------------------------------------------------------------------------------

v_total_zonales <- db2 %>% arrange(Id_Dom) %>% group_by(Zonal) %>% 
  summarise(n = sum(Tamanio)) 

r_f <- list() 
for (i in c(1:4)){
  v_2 <- db2 %>% filter(Zonal==v_total_zonales$Zonal[i])%>% arrange(Id_Dom)
  r <- fun_4(v_total_zonales$n[i],v_2$Tamanio)
  r <- as.data.frame(r)
  r <- cbind("Id_Dom"=c(v_2$Id_Dom),
             "Dominio"=c(v_2$dominio),r)
  r <- r %>%adorn_totals(where =c("col","row"),fill = "-")
  r_f[[i]] <- r
  
}

muestra_final <- rbind(r_f[[1]],r_f[[2]],
                       r_f[[3]],r_f[[4]]) %>% 
  filter(Id_Dom!="Total") %>% arrange(Id_Dom) %>% 
  select(Id_Dom,Dominio,Total)

muestra_final <- rbind(muestra_final,n_final %>% filter(Id_Dom=="20")  %>% 
        rename("Dominio"=nombre_dom,"Total"=Tam_final)) %>% 
  arrange(Id_Dom)



# ------------------------------------------------------------------------------
# EXPORTANDO EXCEL
# ------------------------------------------------------------------------------

wb1 <- createWorkbook()
addWorksheet(wb1,"CENTRO")
addWorksheet(wb1,"LITORAL")
addWorksheet(wb1,"NORTE")
addWorksheet(wb1,"SUR")
addWorksheet(wb1,"NACIONAL")

writeData(wb1,sheet = "CENTRO",r_f[[1]])
writeData(wb1,sheet = "LITORAL",r_f[[2]])
writeData(wb1,sheet = "NORTE",r_f[[3]])
writeData(wb1,sheet = "SUR",r_f[[4]])
writeData(wb1,sheet = "NACIONAL",muestra_final)

ruta <- "RUTINAS/EJERCICIOS PRESUPUESTO/RESULTADOS/ESP_TEMP_PRESUPUESTO_1_6.xlsx"
saveWorkbook(wb1, ruta)






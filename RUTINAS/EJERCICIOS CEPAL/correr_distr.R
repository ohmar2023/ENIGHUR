
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

#m_final <- read.xlsx("Muestra Final_ENVIADA.xlsx") 

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------

db1 <- r

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
# sE CONSIDERAN MULTIPLOS DE 3 SOLO PARA EL LITORAL Y CENTRO
# ------------------------------------------------------------------------------

db1 <- db1 %>% mutate(aux0 = if_else(Zonal=="Litoral"|Zonal=="Centro",1,0),
               aux1 = Tam_final%%3,
               aux2 = if_else(aux1==2,2,0),
               aux3 = if_else(aux1==1,1,0),
               Tamanio2 = if_else(aux0==1,-aux2-aux3+Tam_final,Tam_final)) 
db1 %>% group_by(Zonal) %>% summarise(sum(Tamanio2))

# ------------------------------------------------------------------------------
# ARMANDO 24 DOMINIOS
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

# ------------------------------------------------------------------------------
# USANDO FUNCIONES
# ------------------------------------------------------------------------------

v_total_zonales <- db2 %>% group_by(Zonal) %>% summarise(n = sum(Tamanio))

v_2 <- db2 %>% filter(Zonal=="Norte")  

for (i in c(2:4))
{
  i=3
  r_1 <- fun_1(v_total_zonales$n[i])
  r_2 <- fun_2(v_2$Tamanio,r_1[2])
}

for (j in c(1:dim(v_2)[1]))
{
  if(j==1)
  {
    k = 1
    r_3 <- fun_3(v_2$Tamanio[1],k)
  }else{
    k=
  }
  r_3 <- fun_3(v_2$Tamanio[i],k)
}






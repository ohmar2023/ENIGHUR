library(stringr)

viviendas_por_estrato <- read_excel("PRODUCTOS/DISTRIBUCION/viviendas por estrato.xlsx")

n_1 <- tamanio_area$PSUinSample[1] 
n_2 <- tamanio_area$PSUinSample[2]
v_aux <- as.character(c(26:33))

viviendas_por_estrato <- viviendas_por_estrato %>% mutate(area = substr(estrato,3,3))

viviendas_por_estrato <- viviendas_por_estrato %>% 
                        mutate(id_dom = substr(estrato,1,2)) 

propr_viv_estratos_urb <- viviendas_por_estrato %>% filter(area==1) %>% 
                          mutate(proporcion_prov = num_viviendas/sum(num_viviendas),
                                 n=ceiling(n_1*proporcion_prov),
                                 n=if_else(n<=1,2,n)) 
            
propr_ciu_aux <- viviendas_por_estrato %>% filter(id_dom>24) %>% 
  group_by(id_dom) %>% 
  mutate(proporcion_ciu = num_viviendas/sum(num_viviendas))

propr_viv_estratos_urb <- propr_viv_estratos_urb %>% left_join(select(propr_ciu_aux,
                                            estrato,
                                            proporcion_ciu),
                                     by=c("estrato","id_dom"))
  
propr_viv_estratos_rur <- viviendas_por_estrato %>% filter(area==2) %>% 
                          mutate(proporcion_prov = num_viviendas/sum(num_viviendas),
                                 n=ceiling(n_2*proporcion_prov),
                                 n=if_else(n<=1,2,n))

# -------------------------------------------------------------------------
# CREAMOS EL ID PARA EMPATAR LOS DOMINIOS CON LA BASE DE VIVIENDAS --------
# -------------------------------------------------------------------------
ruta <-"RUTINAS/EJERCICIOS PRESUPUESTO/RESULTADOS/muestra_ejer_presupuesto_estimaciones_2012.xlsx"
muestra_ciu <- read_excel(ruta)
muestra_ciu$id_dom <- str_pad(as.character(c(1:33)),width = 2,side = "left", pad = "0")
v_aux <- as.character(c(25:33))

n_urbano_final <- propr_viv_estratos_urb %>% left_join(select(muestra_ciu,
                                            PSUinSample,
                                            nombre_dom,id_dom),by="id_dom")

n_urbano_final <- n_urbano_final %>% mutate(n=if_else(id_dom %in% v_aux,
                                    ceiling(PSUinSample*proporcion_ciu),n))

n_urbano_final <- n_urbano_final %>% filter(nombre_dom!="Galápagos") %>% 
  group_by(id_dom,nombre_dom) %>% 
  summarise(Tam_final=sum(n)) 

n_rural_final <- propr_viv_estratos_rur %>% 
  left_join(select(n_urbano_final,id_dom,nombre_dom),by="id_dom") %>% 
    group_by(id_dom,nombre_dom) %>% 
    summarise(Tam_final=sum(n)) 

n_final <- rbind(n_urbano_final,n_rural_final) %>%  
  group_by(id_dom,nombre_dom) %>% 
  summarise(Tam_final=sum(Tam_final))  %>% 
  rename(Id_Dom=id_dom) 

n_final[n_final$Id_Dom=="20",3] <- 104
n_final[n_final$Id_Dom=="20",2] <- "Galápagos"

n_final <- n_final %>% mutate(Tam_final = if_else(Tam_final<13,13,Tam_final))

n_final <- n_final %>% adorn_totals()                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
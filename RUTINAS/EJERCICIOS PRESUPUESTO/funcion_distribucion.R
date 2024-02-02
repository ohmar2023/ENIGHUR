distribucion_prov_ciu <- function(base,muestra,prov_var)
{
  #prov_var <- "11"
  # buscando el tamaño de la muestra para la ciudad auto
  n_ciu <- 0
  aux <- viviendas_por_estrato %>%  mutate(id_aux = substr(estrato,1,2)) %>% 
         filter(id_dom==prov_var)
  c_aux <- unique(aux$id_aux)
  entrada_ciu <- c_aux[c_aux!=prov_var]
  if( length(entrada_ciu) != 0 )
  {
    aux_2 <- muestra %>% filter(id_dom==entrada_ciu) %>% select(PSUinSample)
    n_ciu <- aux_2$PSUinSample
    n_ciu <- if_else(is.na(n_ciu),0,n_ciu)
  }
  
  # buscando el tamaño de la muestra para la provincia
  
  aux_3 <- muestra %>% filter(id_dom==prov_var) %>% select(PSUinSample)
  n_prov <- aux_3$PSUinSample
  n_prov <- if_else(is.na(n_prov),0,n_prov)
  
  # Elaboracion algoritmo distribucion 
  
  paso_1 <- base %>% filter(id_dom==prov_var) %>% 
    mutate(proporcion_prov = num_viviendas/sum(num_viviendas),
           id_aux = substr(estrato,1,2))
  
  paso_2 <- paso_1 %>% filter(id_dom == prov_var & 
                                id_dom != id_aux) %>% 
    mutate(proporcion_ciu = num_viviendas/sum(num_viviendas)) %>% 
    select(estrato,proporcion_ciu) %>% 
    full_join(paso_1,by="estrato") %>% 
    mutate(proporcion_ciu = if_else(is.na(proporcion_ciu),0,
                                    proporcion_ciu))
  # todas las proporciones a nivel de la povincia
  paso_3 <- paso_2 %>% mutate(asignacion_prov = ceiling(n_prov*proporcion_prov),
                              asignacion_prov = if_else(asignacion_prov==1,2,asignacion_prov),
                              asignacion_ciu = ceiling(n_ciu*proporcion_ciu),
                              asignacion_ciu = if_else(asignacion_ciu==1,2,asignacion_ciu),
                              asignacion_final = if_else(asignacion_prov>asignacion_ciu,
                                                         asignacion_prov,asignacion_ciu) )%>% 
    group_by(id_aux) %>% summarise(Tam_final=sum(asignacion_final)) %>% 
    rename("Id_Dom"=id_aux)
  paso_3
}





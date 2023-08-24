# Cálculo de TNR ----------------------------------------------------------

base_cobertura <- read_sav("INSUMOS/01 ENIGHUR11_IDENTIFICACION.SAV") %>% clean_names()
base_cobertura <- base_cobertura %>% select(provincia,identif_2010,
                                            identif_hog,
                                             c(44:48))
base_cobertura[is.na(base_cobertura)] <- 10000

cobertura_1 %>% filter(identif_2010=="010150017003") %>% View()

cobertura_1 <- base_cobertura %>% 
  mutate(id_viv = substr(identif_hog,1,14)) %>% 
  #filter(id_viv=="01015001700306")
  group_by(provincia,identif_2010,id_viv) %>%
  summarise(rvo = min(as.numeric(rvo)),
            rvr1 = min(as.numeric(rvr1),na.rm = T),
            rvr2 = min(as.numeric(rvr2),na.rm = T),
            rvr3 = min(as.numeric(rvr3),na.rm = T),
            rvr4 = min(as.numeric(rvr4),na.rm = T)) %>% 
  mutate(rvr1 = as.character( if_else(rvr1 %in% c(99,10000) ,NA,rvr1)),
         rvr2 = as.character(if_else(rvr2 %in% c(99,10000),NA,rvr2)),
         rvr3 = as.character(if_else(rvr3 %in% c(99,10000) ,NA,rvr3)),
         rvr4 = as.character(if_else(rvr4 %in% c(99,10000) ,NA,rvr4))) %>% 
  group_by(identif_2010) %>% 
  summarise(viv_totales= sum(!is.na(rvo)) + sum(!is.na(rvr1))+
              sum(!is.na(rvr2)) + sum(!is.na(rvr3)) + sum(!is.na(rvr4)),
            viv_efectivas = sum(rvo==1,na.rm = T)+sum(rvr1==1,na.rm = T)+
              sum(rvr2==1,na.rm = T)+sum(rvr3==1,na.rm = T)+
              sum(rvr4==1,na.rm = T))

cobertura_2 <- cobertura_1 %>% group_by(Prov = substr(identif_2010,1,2)) %>% 
  summarise(viv_efectivas=sum(viv_efectivas),
            viv_totales=sum(viv_totales),               
            TNR=100-100*(viv_efectivas/viv_totales)) 

mean(cobertura_2$TNR)

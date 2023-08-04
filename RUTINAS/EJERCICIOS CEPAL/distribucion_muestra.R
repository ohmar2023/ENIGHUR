# Escenario 1 -------------------------------------------------------------
library(rio)
marco <- import("INSUMOS/20211025_marco_upm.rds")
#prov <- c("01", "08","07","09","11","13","17", "18","23")
ciu_auto <- c("080150","110150","130850","230150")
#         dom: 30,31,32,33
marco1 <- marco %>% 
  mutate(provincia = substr(id_conglomerado,1,2),
         parr = substr(id_conglomerado,1,6),
         dom = ifelse(parr%in%ciu_auto & provincia=="08" & area ==1,30,
                      ifelse(parr%in%ciu_auto & provincia=="11"& area ==1,31,
                             ifelse(parr%in%ciu_auto & provincia=="13"& area ==1,32,
                                    ifelse(parr%in%ciu_auto & provincia=="23"& area ==1,33,dom)))),
         estrato = paste0(dom,area,est)) %>% 
  group_by(provincia,estrato) %>% 
  summarise(num_viviendas=sum(viv)) 

export(marco1, "PRODUCTOS/viviendas por estrato.xlsx")

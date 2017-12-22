
library(tidyverse)
library(foreign)

dat.denuncias <- read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Bases_datos/VICTIMAS.DBF") %>% 
  as.tibble() %>% 
  left_join(read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Catalogos/DELI_FC.DBF"), 
            by = "DELI_FC") %>% 
  left_join(read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Catalogos/RAN_EDAD.DBF"),
            by = "RAN_EDAD") %>% 
  mutate(tipo_del = parse_number(str_sub(DELI_FC, 1, 2)),
         desc_delito = factor(str_trim(DESC_TIP)), 
         edad = factor(str_trim( str_replace_all(DESCRIP, "a\xa4os", ""))),
         sexo_rec = factor(SEXO, levels = c(1,2), labels = c("Hom", "Muj")))
dat.denuncias %>% data.frame %>% head
dat.denuncias %>% dim
dat.denuncias %>% names

dat.denuncias$tipo_del %>% summary
dat.denuncias$desc_delito %>% unique()
dat.denuncias$edad %>% unique()

cache("dat.denuncias")

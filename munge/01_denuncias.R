
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


# 2013 a 2016
files.vec <- list.files("data/censo_trib_superior/") 
files.sel <- files.vec[str_detect(files.vec, "^ap_")]
files.sel


delitos.l <- lapply(filse.sel, function(nom){
  path <- paste0("data/censo_trib_superior/",nom, 
                 "/Bases_datos/VICTIMAS.DBF")
  dat.denuncias <- read.dbf(file = path) %>% 
    as.tibble() %>% 
    mutate(year = parse_number(nom)-1) 
})

lapply(delitos.l, head)


tab_2013 <- delitos.l[[1]] %>% 
  filter(DEL_FCOM %in% c(302, 303, 304)) %>% 
  group_by(year, RAN_EDAD, SEXO, DEL_FCOM) %>% 
  summarise(victimas = sum(TT_VICTI, na.rm = T))

tab_2014 <- delitos.l[[2]] %>% 
  filter(DELIT_FC %in% c(304, 305, 306, 307)) %>% 
  group_by(year, RAN_EDAD, SEXO, DELIT_FC) %>% 
  summarise(victimas = sum(TT_VICT, na.rm = T))

tab_2015 <- delitos.l[[3]] %>% 
  filter(DELIT_FC %in% c(305, 306, 307, 308, 310)) %>% 
  group_by(year, RAN_EDAD, SEXO, DELIT_FC) %>% 
  summarise(victimas = sum(TT_VICT, na.rm = T))

tab_2016 <- delitos.l[[4]] %>% 
  filter(DELI_FC %in% c(3040, 3050, 3060, 3070, 3080,
                        6012, 6014, 6021)) %>% 
  group_by(year, RAN_EDAD, SEXO, DELI_FC) %>% 
  summarise(victimas = sum(TT_VICT, na.rm = T))


tab_2013 %>% 
  bind_rows(tab_2014) %>% 
  bind_rows(tab_2015) %>% 
  bind_rows(tab_2016) %>% 
  filter(SEXO == 1) %>% 
  group_by(year, RAN_EDAD) %>% 
  



# ENDIREH 2016

library(tidyverse)
library(foreign)

tab_demos_endireh <- read.dbf("data/endireh/base_datos_endireh2016_dbf/TSDem.DBF") %>% 
  as_tibble()
tab_demos_endireh %>% head

tab_sec_x <- read.dbf("data/endireh/base_datos_endireh2016_dbf/TB_SEC_X.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(ID_VIV:T_INSTRUM, -NOM_MUN,
                FAC_VIV:EST_DIS,
                starts_with("P10_1_"),
                starts_with("P10_2_"),
                starts_with("P10_3_")) %>% 
  left_join(tab_demos_endireh) %>%
  mutate(edad_num = parse_number(EDAD))

tab_sec_x %>% names()
tab_sec_x %>% summary()
tab_sec_x$SEXO %>% table()

tab_sec_xi <- read.dbf("data/endireh/base_datos_endireh2016_dbf/TB_SEC_XI.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(ID_VIV:T_INSTRUM, -NOM_MUN,
                FAC_VIV:EST_DIS,
                starts_with("P11")) %>% 
  left_join(tab_demos_endireh) %>%
  mutate(edad_num = parse_number(EDAD))

tab_sec_xi %>% names()
tab_sec_xi %>% summary()
tab_sec_xi$SEXO %>% table()


tab_sec_xii <- read.dbf("data/endireh/base_datos_endireh2016_dbf/TB_SEC_XII.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(ID_VIV:T_INSTRUM, -NOM_MUN,
                FAC_VIV:EST_DIS,
                starts_with("P12_")) %>% 
  left_join(tab_demos_endireh) %>%
  mutate(edad_num = parse_number(EDAD))
  
tab_sec_xii %>% names()
tab_sec_xii %>% summary()
tab_sec_xii %>% data.frame() %>% head

tab_sec_xii %>% 
  group_by(P12_14, P12_14_C, P12_15AB, P12_15C) %>% 
  tally()

tab_sec_xii %>% 
  dplyr::select(P12_14, P12_14_C, P12_15AB, P12_15C) %>% 
  filter(!is.na(P12_14_C))

cache("tab_sec_x")
cache("tab_sec_xi")
cache("tab_sec_xii")

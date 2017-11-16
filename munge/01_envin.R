

library(tidyverse)
library(foreign)

tab_demos <- read.dbf("data/envin/envin_07_dbf/tenvin_sdem.dbf") %>% 
  as_tibble()
tab_demos %>% names()
tab_demos %>% data.frame() %>% head

tab_viv1 <- read.dbf("data/envin/envin_07_dbf/tenvin_viv.dbf") %>% 
  as_tibble()
tab_viv1 %>% data.frame() %>% head

tab_mjov1 <- read.dbf("data/envin/envin_07_dbf/tenvin_mjovenes1.dbf") %>% 
  as_tibble() %>% 
  left_join(tab_demos) %>% 
  mutate(edad_num = parse_number(EDAD))
tab_mjov1 %>% names()
tab_mjov1 %>% data.frame() %>% head()
tab_mjov1 %>% summary()

tab_mjov1$edad_num %>% summary


tab_mjov2 <- read.dbf("data/envin/envin_07_dbf/modulo2_envin07/tenvin_mjovenes2.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(N_CON:N_REN, 
                starts_with("P9_")) %>% 
  left_join(tab_demos) %>% 
  mutate(edad_num = parse_number(EDAD))
tab_mjov2 %>% names()
tab_mjov2 %>% summary()

tab_mjov2$edad_num %>% summary

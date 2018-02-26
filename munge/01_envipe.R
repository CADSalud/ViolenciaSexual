

library(tidyverse)
library(foreign)


# ENVIPE 2017
tab_demos_envipe <- read.dbf("data/envipe/bd_envipe2017_dbf/TSDem.dbf") %>% 
  as_tibble()
tab_demos_envipe %>% head


tab <- tab_demos_envipe %>% 
  group_by(CVE_ENT, CVE_MUN) %>% 
  tally()
tab$n %>% summary

tab_demos_envipe %>% 
  group_by(CVE_ENT) %>% 
  summarise(n = n())

tab_vic2 <- read.dbf("data/envipe/bd_envipe2017_dbf/TPer_Vic2.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(ID_VIV:CVE_MUN, 
                FAC_HOG:UPM_DIS,
                starts_with("AP7_")) %>% 
  gather(preg, value, c(starts_with("AP7_3"), starts_with("AP7_4"))) %>% 
  separate(preg, c('seccion', 'preg_num', 'situacion'), sep = "_") %>% 
  mutate(preg_num = factor(preg_num, 3:4, c("ocurrió", "num_veces"))) %>% 
  spread(preg_num, value) %>% # 1 = Sí
  mutate(edad_num = parse_number(EDAD))
tab_vic2 %>% names()
tab_vic2 %>% data.frame %>% head
tab_vic2 %>% summary()


tab_modvic <- read.dbf("data/envipe/bd_envipe2017_dbf/TMod_Vic.dbf") %>% 
  as_tibble()
tab_modvic %>% names()
tab_modvic %>% data.frame %>% head

names(tab_vic2)
names(tab_modvic)


cache("tab_vic2")
cache("tab_modvic")
cache("tab_demos_envipe")



# 2013 a 2017

files <- list.files("data/envipe/")
files.bd <- files[str_detect(files, "^bd_envipe")]

df_vic2_envipe <- lapply(files.bd, function(file.nom){
  print(file.nom)
  nom.vic <- "TPer_Vic2.dbf"
  if(file.nom == "bd_envipe2013_dbf"){nom.vic <- "tper_vic.dbf"}
  print(nom.vic)
  
  read.dbf(paste0("data/envipe/", file.nom, "/", nom.vic)) %>% 
    as_tibble() %>% 
    dplyr::select(UPM, VIV_SEL, HOGAR, R_SEL,
                  FAC_ELE,
                  starts_with("AP7_")) %>%
    left_join(read.dbf(paste0("data/envipe/", file.nom, "/TSDem.dbf")) %>% 
                as_tibble() %>% 
                dplyr::select(UPM, VIV_SEL, HOGAR, R_SEL = N_REN, 
                              SEXO, EDAD), 
              by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) %>% 
    gather(preg, value, c(starts_with("AP7_3"), starts_with("AP7_4"))) %>% 
    separate(preg, c('seccion', 'preg_num', 'situacion'), sep = "_") %>% 
    mutate(preg_num = factor(preg_num, 3:4, c("ocurrió", "num_veces"))) %>% 
    spread(preg_num, value) %>% # 1 = Sí
    mutate(edad_num = parse_number(as.character(EDAD)), 
           FAC_ELE = parse_number(FAC_ELE), 
           file.nom = file.nom, 
           year = parse_number(file.nom)-1)
  }) %>% 
  bind_rows()

df_vic2_envipe
df_vic2_envipe %>% summary
df_vic2_envipe$year %>% summary


cache("df_vic2_envipe")
# df_vic2_envipe %>% 
#   filter(year == 2012) %>% 
#   write.csv("doc/df_vic2_2012.csv", row.names = F)
# gsutil cp df_vic2_2012.csv gs://envipe

df_vic2_envipe %>% 
  mutate(edad_n = parse_number(as.character(EDAD))) %>% 
  filter(edad_n > 39, 
         ocurrió == 1,
         situacion == 14) %>% 
  group_by(year, edad_n) %>% 
  tally() %>% 
  spread(edad_n, n) %>% 
  data.frame()
    

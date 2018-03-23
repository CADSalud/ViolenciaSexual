

library(ProjectTemplate)
load.project()

# Datos de envin ----

source("munge/01_envin.R")

tab_mjov2$P9_4 %>% unique


df_mod <- tab_mjov2 %>% 
  mutate(intento = factor(P9_1, c(1, 2, 9), c("si", "no", "ne")),
         intento_quien = factor(P9_3, 
                                c('01', '02', '03', '04',
                                  '05', '06', '07', '08', 
                                  '09', '10'),
                                c("Novio(a)","Madre, madrastra", 
                                  "Padre, padrastro", "Hermano(a)",
                                  "Vecino(a)","Tío(a)", "Maestro(a)",
                                  "Sacerdote, cura, ministro",
                                  "Desconocido", "Otra persona")),
         intento_edad = ifelse(parse_number(P9_4)>= 99, 
                               NA, parse_number(P9_4)),
         intento_lugar = factor(P9_5, 
                                c(1:4, 9),
                                c('En la calle', 'En mi casa', 'En la escuela', 
                                  'En otro lugar', 'No especificado')),
         agresion = factor(P9_6, c(1, 2, 9), c("si", "no", "ne")),
         agresion_quien = factor(P9_8, 
                                c('01', '02', '03', '04',
                                  '05', '06', '07', '08',
                                  '09', '10'),
                                c("Novio(a)","Madre, madrastra", 
                                  "Padre, padrastro", "Hermano(a)",
                                  "Vecino(a)","Tío(a)", "Maestro(a)",
                                  "Sacerdote, cura, ministro",
                                  "Desconocido", "Otra persona")),
         agresion_edad = ifelse(parse_number(P9_9)>= 99, 
                               NA, parse_number(P9_9)),
         agresion_lugar = factor(P9_10, 
                                c(1:4, 9),
                                c('En la calle', 'En mi casa', 'En la escuela', 
                                  'En otro lugar', 'No especificado'))) %>% 
  dplyr::select(N_CON:N_REN, intento:agresion_lugar) %>% 
  left_join(tab_demos_envin, 
            by = c("N_CON", "V_SEL", "N_REN"))
df_mod

df_mod %>% data.frame() %>% head()  

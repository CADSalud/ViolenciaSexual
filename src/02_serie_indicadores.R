

library(ProjectTemplate)
load.project()

library(bigrquery)

# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here




df_envipe <- lapply(2013:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT year, SEXO, edad_num, seccion, ",
                "situacion, ocurri__, SUM(FAC_ELE), COUNT(year) ", 
                "FROM [acoustic-field-186719:envipe.vic2_", 
                year.nom,
                "] WHERE situacion = 14 ", # violacion sexual
                "GROUP BY year, SEXO, edad_num, seccion, situacion, ocurri__")
  tt <- query_exec(sql, project = project) 
  }) %>% 
  bind_rows() %>% 
  mutate(edad_gpo = cut(edad_num, breaks = c(seq(19,97, by = 5), 97), 
                        include.lowest = T)) %>% 
  dplyr::rename(nfac = f0_, n = f1_, ocurrencia = ocurri__) %>% 
  as_tibble()

df_envipe
df_envipe %>% 
  group_by(edad_gpo) %>% 
  tally
  
tab <- df_envipe %>% 
  filter(SEXO == 2) %>% 
  group_by(edad_gpo, year, ocurrencia) %>% 
  summarise(nfac = sum(nfac)) %>% 
  ungroup

tab %>% 
  filter(ocurrencia == 1, 
         !is.na(edad_gpo)) %>% 
  ggplot(aes(x = edad_gpo, y = nfac, 
             color = factor(year), 
             group = year)) + 
  geom_point() + 
  geom_line()

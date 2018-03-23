

library(ProjectTemplate)
load.project()

load("cache/poblacion.RData")

library(bigrquery)

# 
# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here

df_envipe <- lapply(2012:2016, function(year.nom){
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
  mutate(edad_gpo = cut(edad_num, breaks = c(seq(19,65, by = 5), 97), 
                        include.lowest = F)) %>% 
  dplyr::rename(nfac = f0_, n = f1_, ocurrencia = ocurri__) %>% 
  as_tibble()

df_envipe
df_envipe %>% 
  group_by(edad_gpo) %>% 
  tally
  
query_exec("select * FROM [acoustic-field-186719:envipe.vic2_2012] limit 3", 
           project = project) 

df_envipe_id <- lapply(2012:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT UPM, VIV_SEL, HOGAR, R_SEL, FAC_ELE, SEXO, ",
                "edad_num, year, situacion, ocurri__ ",
                "FROM [acoustic-field-186719:envipe.vic2_", 
                year.nom,
                "] WHERE situacion = 14 and SEXO = 2")
  tt <- query_exec(sql, project = project) %>% 
    as_tibble()
  }) %>% 
  bind_rows() %>% 
  rename(ocurrencia = ocurri__) %>% 
  unite(id_persona, c('UPM', 'VIV_SEL', 'HOGAR', 'R_SEL')) %>% 
  mutate(edad_gpo = cut(edad_num, breaks = c(seq(19,65, by = 5), 97), 
                        include.lowest = F))
df_envipe_id


# tablas necesarias ----
tab_envipe <- df_envipe %>% 
  filter(SEXO == 2) %>% 
  group_by(edad_gpo, year, ocurrencia) %>% 
  summarise(nfac = sum(nfac)) %>% 
  group_by(edad_gpo, year) %>% 
  mutate(pob_envipe = sum(nfac)) %>% 
  ungroup
  
tab_envipe

tab_envipe %>% 
  filter(ocurrencia == 1, 
         !is.na(edad_gpo)) %>% 
  ggplot(aes(x = edad_gpo, y = nfac, 
             color = factor(year), 
             group = year)) + 
  geom_point() + 
  geom_line()

poblacion$edad %>% summary()
tab_conapo <- poblacion %>% 
  filter(edad > 19, 
         anio > 2011) %>% 
  mutate(edad_gpo = cut(edad, breaks = c(seq(19,65, by = 5), 97), 
                        include.lowest = F), 
         year = parse_number(anio)) %>% 
  filter(sexo == "Mujeres") %>% 
  group_by(sexo, edad_gpo, year) %>% 
  summarise(pob_conapo = sum(pob)) %>% 
  ungroup()
tab_conapo

tab_envipe %>% head
tab_conapo %>% head



# proporcion de abuso
tab_orig <- tab_envipe %>% 
  left_join(tab_conapo, 
            by = c("edad_gpo", "year")) %>% 
  na.omit() %>% 
  mutate(prop_envipe = nfac/pob_envipe, 
         prop_conapo = nfac/pob_conapo)

tab_orig %>% 
  filter(year == 2012, 
         ocurrencia == 1)

# Compare population estimation ----
# Proportion using conapo population and envipe population
tab_orig %>% 
  filter(ocurrencia == 1) %>% 
  ggplot(aes(x = prop_envipe, y = prop_conapo, 
             color = factor(year))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(labels = function(x)100000*x) +
  scale_y_continuous(labels = function(x)100000*x) +
  facet_wrap(~edad_gpo, scales = "free")



# Remuestreo----
prop_fun <- function(sub){
  sub %>% 
    group_by(year, edad_gpo, ocurrencia) %>% 
    summarise(n_fac = sum(FAC_ELE)) %>% 
    group_by(year, edad_gpo) %>% 
    mutate(prop_envipe = n_fac/sum(n_fac)) %>% 
    ungroup() %>% 
    left_join(tab_conapo, 
              by = c("year", "edad_gpo")) %>% 
    mutate(prop_conapo = n_fac/pob_conapo)
}


tab_boot <- df_envipe_id %>% 
  filter(!is.na(edad_gpo)) %>% 
  group_by(year) %>% 
  bootstrap(m = 100, by_group = T) %>% 
  do(prop_fun(.)) %>% 
  ungroup
tab_boot


tab_boot %>% 
  filter(ocurrencia == 1) %>% 
  ggplot(aes(x = prop_envipe, 
             color = factor(year))) + 
  geom_density() + 
  facet_wrap(~edad_gpo, scales ="free")

tab_boot %>% 
  filter(ocurrencia == 1) %>% 
  ggplot(aes(x = edad_gpo, 
             y = prop_envipe,
             fill = factor(year))) + 
  geom_boxplot()

summ_envipe <- tab_boot %>% 
  filter(ocurrencia == 1) %>% 
  dplyr::select(replicate, year, edad_gpo, n_fac, prop = prop_envipe) %>% 
  gather(tipo, val, n_fac, prop) %>% 
  group_by(year, edad_gpo, tipo) %>% 
  summarise(prom = mean(val), 
            median = median(val), 
            q75 = quantile(val, .75), 
            q25 = quantile(val, .25)) %>% 
  ungroup %>% 
  arrange(tipo)
summ_envipe

cache("summ_envipe")

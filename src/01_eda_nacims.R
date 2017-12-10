

library(ProjectTemplate)
reload.project()

library(bigrquery)

# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here

# Query de tablas ----
tab.nacims <- lapply(2010:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT  COUNT(filenom), ANO_REG, EDAD_MADR, EDAD_PADR FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] group by ANO_REG, EDAD_MADR, EDAD_PADR")
  tt <- query_exec(sql, project = project)
  }) %>% 
  bind_rows() %>% 
  as.tibble() %>% 
  rename(count = f0_)
tab.nacims
str(tab.nacims)

tab.nacims$EDAD_MADR %>% summary()

tab <- tab.nacims %>% 
  filter(EDAD_MADR < 50) %>%
  mutate(edad_gpo = cut(EDAD_MADR, 
                        right = F,
                        breaks = seq(10, 100, by = 5), 
                        include.lowest = T)) %>% 
  group_by(ANO_REG, edad_gpo) %>% 
  summarise(n = sum(count)) %>% 
  ungroup()
tab$edad_gpo %>% summary()
tab$ANO_REG %>% summary()
tab

gg <- tab %>% 
  ggplot(aes(x = edad_gpo, y = n, 
             color = (ANO_REG),
             group = (ANO_REG))) + 
  geom_line() + 
  geom_point(size = 2) + 
  # facet_wrap(~edad_gpo, scales = "free_y") + 
  ylab("Frecuencia\n(miles)") + 
  xlab("Edad de la madre") +
  ggtitle("Serie de nacimientos registrados por edad") + 
  scale_y_continuous(labels = function(x)x/1e3) + 
  guides(color = guide_legend("Año"))
gg
input.l$gg_nacims_serieedad <- gg

gg <- tab %>% 
  filter(edad_gpo %in% c("[10,15)", "[15,20)", "[20,25)")) %>% 
  ggplot(aes(x = ANO_REG, y = n)) + 
  geom_line(color = "gray60") + 
  geom_point(size = 2) + 
  facet_wrap(~edad_gpo, scales = "free_y") +
  ylab("Frecuencia\n(miles)") + 
  xlab("Año de registro") +
  ggtitle("Serie de nacimientos registrados por edad") + 
  scale_y_continuous(labels = function(x)x/1e3) + 
  guides(color = guide_legend("Grupo de edad"))
gg
input.l$gg_nacims_serie <- gg

cache("input.l")

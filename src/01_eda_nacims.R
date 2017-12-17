

library(ProjectTemplate)
reload.project()

library(bigrquery)

# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here

# Query de tablas ----
df_nacims <- lapply(2010:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT  COUNT(filenom), ANO_REG, EDAD_MADR", 
                ", EDAD_PADR FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] group by ANO_REG, EDAD_MADR, EDAD_PADR")
  tt <- query_exec(sql, project = project)
  }) %>% 
  bind_rows() %>% 
  as.tibble() %>% 
  rename(count = f0_) %>% 
  mutate(EDAD_MADR = ifelse(EDAD_MADR == 99, NA, EDAD_MADR),
         EDAD_PADR = ifelse(EDAD_PADR == 99, NA, EDAD_PADR),
         edad_gpo_madre = cut(ifelse(EDAD_MADR == 99, NA, EDAD_MADR), 
                              right = F,
                              breaks = seq(10, 100, by = 5), 
                              include.lowest = T),
         edad_gpo_padre = cut(ifelse(EDAD_PADR == 99, NA, EDAD_PADR), 
                              right = F,
                              breaks = seq(12, 80, by = 6), 
                              include.lowest = T), 
         edad_gpo_madre = fct_explicit_na(edad_gpo_madre),
         edad_gpo_padre = fct_explicit_na(edad_gpo_padre, na_level = "(No registrado)")) 
df_nacims
str(df_nacims)

df_nacims$EDAD_MADR %>% summary()

tab <- df_nacims %>% 
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


# grafica de edad
col.gradient.cut <- c(colorRampPalette(c("#9999ff", "yellow", "#E67400"))(11), "gray50")

(df_nacims$EDAD_PADR == 99) %>% sum
(df_nacims$EDAD_PADR ) %>% summary()
df_nacims %>% 
  # filter(EDAD_MADR < 50) %>% 
  group_by(edad_gpo_madre, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(edad_gpo_madre) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup %>% 
  ggplot(aes(x = edad_gpo_madre, 
             y = prop, 
             fill= edad_gpo_padre)) + 
  geom_bar(stat = "identity") + 
  # geom_hline(yintercept = 50, color = "gray50", linetype = 2) +
  scale_fill_manual(values = col.gradient.cut) + 
  ylab("Porcentaje (%)") + 
  xlab("Edad de la madre") +
  guides(fill = guide_legend("Edad del\npadre")) +
  ggtitle("Proporción de registros por edad del padre dado edad de la madre",
          "Número de registros 2010 a 2015") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

tab <- df_nacims %>% 
  filter(EDAD_MADR < 50) %>%
  mutate(EDAD_MADR = ifelse(EDAD_MADR %in% c(10,11,12), 12, EDAD_MADR)) %>% 
  filter(!is.na(EDAD_MADR)) %>%
  group_by(EDAD_MADR, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(EDAD_MADR) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup 

tab %>% 
  ggplot(aes(x = EDAD_MADR, 
             y = prop, 
             fill= edad_gpo_padre)) + 
  geom_bar(stat = "identity") + 
  # geom_hline(yintercept = 50, color = "gray50", linetype = 2) +
  scale_fill_manual(values = col.gradient.cut) + 
  ylab("Porcentaje (%)") + 
  xlab("Edad de la madre") +
  guides(fill = guide_legend("Edad del\npadre")) +
  ggtitle("Proporción de registros por edad del padre dado edad de la madre",
          "Número de registros 2010 a 2015") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


df_nacims %>% 
  ggplot(aes(x = EDAD_MADR)) + 
  geom_histogram()

df_nacims %>% 
  group_by(EDAD_MADR) %>% 
  summarise(base = sum(count)) %>% 
  ungroup %>% 
  ggplot(aes(x = EDAD_MADR, y = base)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = seq(0, 100, by = 5)) + 
  xlim(10,100)
 
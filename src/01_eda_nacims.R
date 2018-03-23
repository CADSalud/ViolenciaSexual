

library(ProjectTemplate)
# load.project()

library(bigrquery)

# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here
query_exec(query = "SELECT filenom, ANO_REG, EDAD_MADN, EDAD_PADN FROM [acoustic-field-186719:nacimientos.nacim2015] LIMIT 10", 
           project = project)

# Query de tablas ----
df_nacims <- lapply(2010:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT  COUNT(filenom), ANO_REG, EDAD_MADN", 
                ", EDAD_PADN FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] group by ANO_REG, EDAD_MADN, EDAD_PADN")
  tt <- query_exec(sql, project = project)
  }) %>% 
  bind_rows() %>% 
  as.tibble() %>% 
  rename(count = f0_) %>% 
  mutate(EDAD_MADN = ifelse(EDAD_MADN == 99, NA, EDAD_MADN),
         EDAD_PADN = ifelse(EDAD_PADN == 99, NA, EDAD_PADN),
         edad_gpo_madre = cut(ifelse(EDAD_MADN == 99, NA, EDAD_MADN), 
                              right = F,
                              breaks = seq(10, 100, by = 5), 
                              include.lowest = T),
         edad_gpo_padre = cut(ifelse(EDAD_PADN == 99, NA, EDAD_PADN), 
                              right = F,
                              breaks = seq(12, 80, by = 6), 
                              include.lowest = T), 
         edad_gpo_madre = fct_explicit_na(edad_gpo_madre),
         edad_gpo_padre = fct_explicit_na(edad_gpo_padre, na_level = "(No registrado)")) 
df_nacims
str(df_nacims)

df_nacims$EDAD_MADN %>% summary()

cache("df_nacims")

tab <- df_nacims %>% 
  filter(EDAD_MADN < 50) %>%
  mutate(edad_gpo = cut(EDAD_MADN, 
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

(df_nacims$EDAD_PADN == 99) %>% sum
(df_nacims$EDAD_PADN ) %>% summary()

tab <- df_nacims %>% 
  # filter(EDAD_MADN < 50) %>% 
  group_by(edad_gpo_madre, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(edad_gpo_madre) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup
tab
input.l$tab_distquinq_nacims <- tab

gg <- tab %>% 
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
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90))
gg
input.l$gg_distquinq_nacims <- gg

tab <- df_nacims %>% 
  filter(EDAD_MADN < 50) %>%
  mutate(EDAD_MADN = ifelse(EDAD_MADN %in% c(10,11,12), 12, EDAD_MADN)) %>% 
  filter(!is.na(EDAD_MADN)) %>%
  group_by(EDAD_MADN, edad_gpo_padre) %>% 
  summarise(n_acum = sum(count)) %>% 
  group_by(EDAD_MADN) %>% 
  mutate( prop = 100*n_acum/sum(n_acum)) %>% 
  ungroup%>% 
  mutate(EDAD_MADN = fct_recode(factor(EDAD_MADN), 
                                `10 a 12` = "12") )
tab
input.l$tab_distanual_nacims <- tab

gg <- tab %>% 
  ggplot(aes(x = EDAD_MADN, 
             y = prop, 
             fill= edad_gpo_padre)) + 
  geom_bar(stat = "identity") + 
  # scale_x_continuous(breaks = seq(12, 50, 2)) + 
  scale_fill_manual(values = col.gradient.cut) + 
  ylab("Porcentaje (%)") + 
  xlab("Edad de la madre") +
  guides(fill = guide_legend("Edad del\npadre")) +
  ggtitle("Proporción de registros por edad del padre dado edad de la madre",
          "Número de registros 2010 a 2015") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90))
gg
input.l$gg_distanual_nacims <- gg



gg <- df_nacims %>% 
  group_by(EDAD_MADN) %>% 
  summarise(base = sum(count)) %>% 
  ungroup %>% 
  ggplot(aes(x = EDAD_MADN, y = base)) + 
  geom_bar(stat = "identity") + 
  # scale_x_continuous(breaks = seq(0, 100, by = 5)) + 
  xlim(10,100)
gg
input.l$gg_distedadmdr_nacims <- gg


cache("input.l")

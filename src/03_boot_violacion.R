

library(ProjectTemplate)
load.project()

load("cache/dist_ensanut12_vs.RData")
load("cache/summ_envipe.RData")
load("cache/dat.denuncias.RData")

graficas_proys_l <- list()

gpos_edad <- c('0 a 4','5 a 9',
  '10 a 14','15 a 19','20 a 24','25 a 29',
  '30 a 34','35 a 39','40 a 44','45 a 49',
  '50 a 54','55 a 59',
  '60  y más','No identificado')

dist_ensanut12_vs_tidy <- dist_ensanut12_vs %>% 
  gather(var, val, -redad) %>% 
  mutate(tipo = ifelse(var %in% c('promn', 'mediann', 'q75n', 'q25n'),
                      "n_fac", "prop"), 
         var = fct_recode(var, prom = 'promn',
                          median = 'mediann', 
                          q75 = 'q75n', 
                          q25 = 'q25n')) 


# Union de envipe y ensanut
tab_envipe <- summ_envipe %>% 
  # filter(tipo == "prop") %>% 
  rename(redad = edad_gpo) %>%
  mutate(enc = "ENVIPE") %>% 
  ungroup %>% 
  gather(var, val, prom:q25)

tab_ensanut <- dist_ensanut12_vs_tidy %>% 
  # filter(tipo == "prop") %>% 
  mutate(year = 2012, 
         enc = "ENSANUT") %>% 
  ungroup 

tab_summ <- tab_envipe %>% 
  bind_rows(tab_ensanut) %>% 
  mutate(redad = factor(redad, 
                        c("adol_temp", "adol_tard", 
                          '(19,24]', '(24,29]', '(29,34]', '(34,39]', 
                          '(39,44]', '(44,49]', '(49,54]', '(54,59]', 
                          '(59,64]', '(64,97]'), 
                        c("10 a 14", "15 a 19", 
                          '20 a 24', '25 a 29', '30 a 34', '35 a 39', 
                          '40 a 44', '45 a 49', '50 a 54', '55 a 59', 
                          '60 a 64', '65 y más')))
tab_summ

tab_summ %>% 
  spread(var, val) %>% 
  ggplot(aes(x = redad, y = median, 
             color = enc)) + 
  geom_point() + 
  geom_point(aes(y = prom), shape = 4, size = 4) + 
  geom_linerange(aes(ymin = q25, ymax = q75)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(tipo ~ year, scales = "free_y")

tab_summ %>% 
  spread(enc, val) %>% 
  ggplot(aes(x = ENSANUT, y = ENVIPE, 
             color = redad)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~var, scales = "free")
  
gg <- tab_summ %>% 
  spread(var, val) %>% 
  filter(year %in% c(2012), 
         tipo == "prop") %>% 
  ggplot(aes(x = redad, y = median, 
             color = enc)) + 
  geom_point() +
  geom_linerange(aes(ymin = q25, ymax = q75)) +
  ylab("tasa de violencia sexual") + 
  xlab("grupos de edad") + 
  guides(color = guide_legend("Encuesta")) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~year, scales = "free_y")
gg
graficas_proys_l$gg_1 <- gg


# diferencia promedio ----

tab_summ %>% 
  filter(tipo == "prop")

dif_summ <- tab_summ %>% 
  spread(enc, val) %>% 
  filter(var == "median", 
         tipo == "prop") %>% 
  na.omit() %>% 
  mutate(diff = ENVIPE - ENSANUT) %>% 
  filter(redad == "20 a 24")
  # group_by(tipo, var) %>% 
  # summarise(prom_diff = mean(diff)) %>% 
  # ungroup()
dif_summ

tab_proy_adol <- tab_summ %>% 
  filter(year == 2012, 
         enc == "ENSANUT", 
         var == "median", 
         tipo == "prop",
         redad %in% c('15 a 19', '10 a 14')) %>% 
  mutate(val = val + dif_summ$diff, 
         enc = "proyeccion") %>% 
  bind_rows(tab_summ %>% 
              filter(tipo == "prop"))
tab_proy_adol %>% 
  filter(var == "median")

gg <- tab_proy_adol %>% 
  filter(year %in% c(2012)) %>%
  spread(var, val) %>% 
  ggplot(aes(x = redad, y = median, 
             color = enc)) + 
  geom_point() +
  geom_linerange(aes(ymin = q25, ymax = q75)) +
  ylab("tasa de violencia sexual") + 
  xlab("grupos de edad") + 
  guides(color = guide_legend("Encuesta")) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~year, scales = "free_y")
gg
graficas_proys_l$gg_2 <- gg

# diferencia 2015 ----
tt <- tab_proy_adol %>% 
  filter(year == 2012, var == "median")
tt
val1 <- 0.00101
val2 <- 0.00181
val3 <- 0.000538  

prop2 <- val3/val2
prop1 <- val3/val1

tab <- tab_proy_adol %>% 
  filter(year == 2015, 
         redad %in% c('20 a 24'),
         var == "median")

tab_proy_15 <- tibble(year = 2015, 
       redad = c('10 a 14', '15 a 19'), 
       enc = "proyección",
       var = "median",
       tipo = "prop",
       val = c(tab$val/prop1, tab$val/prop2)) %>% 
  bind_rows(tab_proy_adol) %>% 
  mutate(redad = factor(redad,
                        c("10 a 14", "15 a 19", 
                          '20 a 24', '25 a 29', '30 a 34', '35 a 39', 
                          '40 a 44', '45 a 49', '50 a 54', '55 a 59', 
                          '60 a 64', '65 y más')))
tab_proy_15  

gg <- tab_proy_15  %>% 
  filter(year %in% c(2015)) %>%
  spread(var, val) %>% 
  ggplot(aes(x = redad, y = median, 
             color = enc)) + 
  geom_point() +
  geom_linerange(aes(ymin = q25, ymax = q75)) +
  ylab("tasa de violencia sexual") + 
  xlab("grupos de edad") + 
  guides(color = guide_legend("Encuesta")) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~year, scales = "free_y")
gg

graficas_proys_l$gg_3 <- gg


# cifra negra ----
tab_summ %>% 
  filter(year == 2015, 
         tipo == "n_fac",
         var == "median")


tab_den <- dat.denuncias %>% 
  # data.frame() %>% head
  filter(DELI_FC %in% c(3040, 3050, 3060, 3070, 3080, 
                        6012, 6014, 6021), 
         SEXO == 2) %>% 
  mutate(redad = factor(RAN_EDAD, 1:14, gpos_edad), 
         year = 2015) %>% 
  group_by(year, redad) %>% 
  summarise(victimas_den = sum(TT_VICT, na.rm = T)) %>% 
  ungroup()

tab_cifneg <- tab_summ %>% 
  filter(year == 2015, 
         tipo == "n_fac",
         var == "median", 
         redad %in% gpos_edad[3:10]) %>% 
  bind_rows(tibble(year = 2015, 
                   redad = c('10 a 14', '15 a 19'), 
                   enc = "proyección",
                   var = "median",
                   tipo = "n_fac",
                   val = c(8355.5/prop1, 9650.0/prop2))) %>% 
  left_join(tab_den) %>% 
  mutate(cifra_neg = 1-victimas_den/val, 
         redad = factor(redad, gpos_edad[3:10])) %>% 
  arrange(redad)
tab_cifneg



gg <- tab_cifneg  %>% 
  ggplot(aes(x = as.numeric(redad))) + 
  geom_ribbon(aes(ymin = 0, ymax = cifra_neg), 
              fill = "indianred2") + 
  geom_hline(yintercept = 1, 
             color = "gray30") + 
  geom_label(aes(y = cifra_neg - .01, 
                label = paste0(round(100*cifra_neg), "%") ), 
             color = "gray30") + 
  scale_x_continuous(breaks = 1:8, 
                     labels = gpos_edad[3:10]) +
  scale_y_continuous(labels = function(x)paste(100*x, "%")) +
  ylab("cifra negra") + 
  xlab("grupos de edad") + 
  facet_wrap(~year, scales = "free_y")
gg

graficas_proys_l$gg_4 <- gg
graficas_proys_l$tab_4 <- tab_cifneg

cache("graficas_proys_l")

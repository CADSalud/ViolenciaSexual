
library(ProjectTemplate)
load.project()

# endireh
load("cache/tab_sec_xii.RData") 
load("cache/input.l.RData") 


# Edad del primer hijo
tab_eprimerh <- tab_sec_xii %>% 
  mutate(edad_primerh = parse_number(P12_2),
         edad_gpo_primerh = cut( edad_primerh,
                                 include.lowest = T,
                                 breaks = c(1, 9, 14, 19, 100)))

tab_eprimerh$FAC_MUJ %>% sum() # 46 millones de mujeres mayores de 15 años
tab <- tab_eprimerh %>% 
  group_by(edad_gpo_primerh) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerh)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac)) 
tab
input.l$tab_gpoedad_primerh_endireh <- tab


gg <- ggplot(tab, aes(x = edad_gpo_primerh, 
                y = prop)) + 
  geom_bar(stat = "identity", position = "stack") + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Grupos de edad") + 
  ggtitle("Distribución de edad al primer hijo.",
          "Mujeres entre 15 y 19 años.")
gg
input.l$gg_gpoedad_primerh_endireh <- gg

  
gg <- tab_eprimerh %>% 
  filter(edad_primerh < 99) %>% 
  ggplot(aes(x = 1, y = edad_primerh)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  ggtitle("Distribución de edad al primer hijo") + 
  coord_flip() + 
  xlab(NULL) + 
  ylab("Edad en años")
gg
input.l$gg_edad_primerh <- gg


tab_sec_xii %>% 
  dplyr::select(starts_with("FAC_"), 
                edad_primerh = P12_2,
                SEXO, EDAD, edad_num) 



# Edad de la primera relacion ----

tab_eprimerarel <- tab_sec_xii %>% 
  mutate(edad_primerarel = parse_number(P12_6),
         edad_gpo_primerarel = cut( edad_primerarel,
                                 include.lowest = T,
                                 breaks = c(5, 9, 14, 19, 100)), 
         consentimiento = factor(P12_7, 1:2, c("sí", "no")))

tt.0 <- tab_eprimerarel %>% 
  group_by(edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac), 
         consentimiento = "total")

tt <- tab_eprimerarel %>% 
  group_by(consentimiento, 
           edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  group_by(consentimiento) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup %>% 
  bind_rows(tt.0)
tt

gg <-
  ggplot(tt, aes(x = edad_gpo_primerarel, 
               y = prop)) + 
  geom_bar(aes(fill = consentimiento),
           stat = "identity", position = "dodge") + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  facet_wrap(~consentimiento) + 
  theme(legend.position = "none") +
  ylab("Proporción (%)") + 
  xlab("Grupo de edad") + 
  ggtitle("Distribución de edad de primera relación", 
          "Consentimiento de la relación")
gg
input.l$gg_gpoedad_primerarel_endireh <- gg


# Edad de primer union ----
tab_conyug_i <- tab_sec_xii %>% 
  mutate(edad_conyug = parse_number(P12_14),
         edad_gpo_conyug = cut( edad_conyug,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_15AB))
tab_conyug_i

tab <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))
tab

gg <- ggplot(tab, aes(x = edad_gpo_conyug, 
                y  = prop)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ggtitle("Distribución de edad de primera unión") + 
  ylab("Proporción (%)") + 
  xlab("Grupo de edad")
gg
input.l$gg_gpoedad_primeraun <- gg


tt <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup
tt

gg <- tt %>% 
  filter(edad_pareja < 75) %>% 
  ggplot(aes(x = edad_pareja, 
             y = prop, 
             color = edad_gpo_conyug)) + 
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Blues") +
  geom_vline(xintercept = 18) + 
  ylab("Proporción (%)") + 
  xlab("Edad de la pareja") + 
  guides(color = guide_legend("Grupo de edad\nmujer")) + 
  ggtitle("Probabilidad de la edad de la pareja.",
          "Grupo de edad de la mujer en la primera union.")
gg
input.l$gg_prop_edadparejaunion_endireh <- gg


gg <- tab_conyug_i %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ggplot(aes(x = edad_gpo_conyug, y = edad_pareja)) + 
  geom_hline(yintercept = 18, color = "blue") +
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  xlab("Grupo de edad") + 
  ylab("Edad de la pareja") + 
  ggtitle("Distribución de la edad de la pareja.",
          "Grupo de edad de la mujer en la primera union.") + 
  coord_flip()
gg
input.l$gg_box_edadparejaunion_endireh <- gg



cache("input.l")

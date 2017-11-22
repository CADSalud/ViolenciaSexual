
library(ProjectTemplate)
load.project()

# endireh
load("cache/tab_sec_xii.RData") 

tab_eprimerh$FAC_MUJ %>% sum() # 46 millones de mujeres mayores de 15 años

# Edad del primer hijo
tab_eprimerh <- tab_sec_xii %>% 
  mutate(edad_primerh = parse_number(P12_2),
         edad_gpo_primerh = cut( edad_primerh,
                                 include.lowest = T,
                                 breaks = c(1, 9, 14, 19, 100)))

tab_eprimerh %>% 
  group_by(edad_gpo_primerh) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerh)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))
  
tab_eprimerh %>% 
  ggplot(aes(x = 1, y = edad_primerh)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  ggtitle("Distribución de edad del primer hijo")

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

tab_eprimerarel %>% 
  group_by(edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))

tt <- tab_eprimerarel %>% 
  group_by(consentimiento, 
           edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  group_by(consentimiento) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup
tt

ggplot(tt, aes(x = edad_gpo_primerarel, 
               y = prop, 
               fill = consentimiento)) + 
  geom_bar(stat = "identity", 
           position = "dodge") + 
  facet_wrap(~consentimiento)



# Edad de primer union ----

tab_conyug_i <- tab_sec_xii %>% 
  mutate(edad_conyug = parse_number(P12_14),
         edad_gpo_conyug = cut( edad_conyug,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_15AB))
tab_conyug_i

tab_conyug_i %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))

tt <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup
tt

tt %>% 
  filter(edad_pareja < 75) %>% 
  ggplot(aes(x = edad_pareja, 
             y = prop, 
             color = edad_gpo_conyug)) + 
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Blues") +
  geom_vline(xintercept = 18)


tt <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup

tab_conyug_i %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ggplot(aes(x = edad_gpo_conyug, y = edad_pareja)) + 
  geom_hline(yintercept = 18, color = "blue") +
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 110, 5)) 



# Edad de vida conyugal ----
tab_conyug <- tab_sec_xii %>% 
  mutate(edad_conyug = parse_number(P12_9),
         edad_gpo_conyug = cut( edad_conyug,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_10),
         edad_parejai = parse_number(P12_10_C),
         edad_gpo_pareja = cut( edad_pareja,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)))
tab_conyug %>% 
  dplyr::select(edad_num:edad_gpo_pareja)

tab_conyug %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))


tt <- tab_conyug %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup
tt

tt %>% 
  filter(edad_pareja < 75) %>% 
  ggplot(aes(x = edad_pareja, 
               y = prop, 
               color = edad_gpo_conyug)) + 
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Blues") +
  geom_vline(xintercept = 18)


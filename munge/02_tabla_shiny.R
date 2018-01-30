
library(tidyverse)

df_resumen <- readxl::read_excel("data/TablaResumenVS_VU.xlsx") %>% 
  dplyr::rename(Year = Año)
df_resumen

cache("df_resumen")


df_resumen %>% 
  dplyr::select(Tema, Indicador) %>% 
  unique() %>% 
  data.frame()


df_resumen$Indicador %>% unique() %>% write.table(row.names = F)
df_resumen$Adolescencia %>% unique()



df_resumen %>% 
  spread(Year, Valor) %>% 
  filter(Indicador == "nosolt", 
         Adolescencia == "[10 a 14]") %>% 
  gather(year, value, -c(Fuente:Adolescencia)) %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_point()
  


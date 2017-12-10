
# Natalidad: Nacimientos INEGI
# Init: 06/01/2017
# SGMC

# 0. Archivos nacimientos
# 1. Defunciones 2010 a 2015

library(foreign)
library(bigrquery)


# Actualizacion
# Proyecto ----
project <- "acoustic-field-186719" # put your project ID here
sql <- paste0("SELECT  * FROM [acoustic-field-186719:nacimientos.nacim2011] limit 5")
df_prueba <- query_exec(sql, project = project) %>% 
  mutate_all(.funs = as.character) 
df_prueba

file.u <- "natalidad_base_datos_2016_dbf"
path.u <- paste0("data/nacimientos/",file.u,"/NACIM16.DBF")
dat <- read.dbf(path.u,
                as.is = T ) %>%
  mutate(filenom = file.u) %>% 
  as.tibble() %>% 
  mutate_all(.funs = as.character) %>%
  bind_rows(df_prueba) %>% 
  filter(filenom == file.u)
write.csv(dat, "data/nacimientos/nacims_2016.csv", row.names = F)




# # Carga inicial ----
# # 0. Archivos nacimientos
# path.str <- "data/nacimientos/"
# files.noms.aux <- list.files(path.str)[!str_detect(list.files(path.str), ".zip")]
# parse_number(files.noms.aux)
# 
# my_db <- src_sqlite(path = "data/nacimientos.db", create = T)
# my_db # tablas
# # my_db$con %>% db_drop_table(table='nacimientos') # This drops the table 'nacimientos'.
# 
# 
# # 1. Defunciones 1998 a 2015
# files.noms <- files.noms.aux[parse_number(files.noms.aux) >= 2010]
# 
# lapply(files.noms, function(file.u){
#   # file.u <- files.noms[2]
#   print(file.u)
#   dbf.files <- list.files( paste0(path.str,file.u))
#   path.u <- paste(path.str, file.u, sep = "/")
#   dat <- read.dbf( paste0(path.u, "/", dbf.files[str_detect(dbf.files, "^NACIM")]),
#                    as.is = T ) %>%
#     mutate(filenom = file.u) %>% 
#     # caracter para evitar problemas en bind_rows
#     mutate_all(.funs = as.character) 
#   }) %>% 
#   bind_rows() %>%
#   db_insert_into( con = my_db$con, table = "nacimientos", values = .) # insert into
# 
# 
# sqlite3 -header -csv nacimientos.db "select * from nacimientos filenom == 'natalidad_base_datos_2010_dbf';" > out.csv
# 
# 


# Natalidad: Nacimientos INEGI
# Init: 06/01/2017
# SGMC

# 0. Archivos nacimientos
# 1. Defunciones 2010 a 2015

# library(foreign)

# 0. Archivos nacimientos
path.str <- "data/nacimientos/"
files.noms.aux <- list.files(path.str)[!str_detect(list.files(path.str), ".zip")]
parse_number(files.noms.aux)

my_db <- src_sqlite(path = "data/nacimientos.db", create = T)
my_db # tablas
# my_db$con %>% db_drop_table(table='nacimientos') # This drops the table 'nacimientos'.


# 1. Defunciones 1998 a 2015
files.noms <- files.noms.aux[parse_number(files.noms.aux) >= 2010]

lapply(files.noms, function(file.u){
  # file.u <- files.noms[2]
  print(file.u)
  dbf.files <- list.files( paste0(path.str,file.u))
  path.u <- paste(path.str, file.u, sep = "/")
  dat <- read.dbf( paste0(path.u, "/", dbf.files[str_detect(dbf.files, "^NACIM")]),
                   as.is = T ) %>%
    mutate(filenom = file.u) %>% 
    # caracter para evitar problemas en bind_rows
    mutate_all(.funs = as.character) 
  }) %>% 
  bind_rows() %>%
  db_insert_into( con = my_db$con, table = "nacimientos", values = .) # insert into


sqlite3 -header -csv nacimientos.db "select * from nacimientos filenom == 'natalidad_base_datos_2010_dbf';" > out.csv



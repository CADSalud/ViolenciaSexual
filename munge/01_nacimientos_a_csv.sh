

sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2010_dbf';" > nacims_2010.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2011_dbf';" > nacims_2011.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2012_dbf';" > nacims_2012.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2013_dbf';" > nacims_2013.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2014_dbf';" > nacims_2014.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2015_dbf';" > nacims_2015.csv
sqlite3 -header -csv nacimientos.db "select * from nacimientos where filenom == 'natalidad_base_datos_2016_dbf';" > nacims_2016.csv
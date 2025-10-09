library(noctua)
library(DBI)
library(readr)


noctua_options(cache_size = 10)

con <- dbConnect(noctua::athena())

data <- dbGetQuery(conn = con, read_file("testing_file.sql"))

##Connect to e-ecology

db.file <-"Robin3" #provide the name of the database or file for ODBC driver
library(RODBC) 
db <- odbcConnect(db.file)#establish connection with database
tables <- sqlTables(db)#view the table names
tracks <- sqlColumns(db, "track")#view specific columns of the table

track <- sqlFetch(db, "m201804.track", max=2000)
sqlQuery(db, "select * from m201804.track")

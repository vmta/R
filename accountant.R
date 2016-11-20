Sys.setlocale("LC_ALL","UTF-8")
setwd("~/Documents/workspace_r")

# Load binary DB data.frame
# load("db.Rdat")
db <- read.table(file = file.choose(), header = FALSE, sep = ";")
colnames(db) <- c('date', 'direction', 'currency', 'amount', 'category', 'location', 'comment')

# Clean whitespaces
db$direction <- substring(db$direction, 2, 255)
db$currency <- substring(db$currency, 2, 255)
db$category <- substring(db$category, 2, 255)
db$location <- substring(db$location, 2, 255)
db$comment <- substring(db$comment, 2, 255)

# Save binary DB data.frame
# save(db, file = "db.Rdat")
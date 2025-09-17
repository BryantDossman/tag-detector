library(RSQLite)
library(DBI)

# Connect to the database
con <- dbConnect(SQLite(), "./tag_db/tag_db.sqlite")

# List tables
dbListTables(con)

# Extract tags table
tags <- dbReadTable(con, "tags")
print(tags)

# Save as CSV
write.csv(tags, "tags.csv", row.names = FALSE)

# Close connection
dbDisconnect(con)

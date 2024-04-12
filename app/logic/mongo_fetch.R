box::use(
  mongolite[mongo]
  )

# connection_string <- "mongodb+srv://dina-user:LZtL1VtD0KTyW4pZ@dina-db.rzr5ytg.mongodb.net/?retryWrites=true&w=majority&appName=dina-db"
# collection <- "test"
# db <- "reports"
fetch_mongodb <- function(connection_string, collection, db) {
  reports <- mongo(collection=collection, db=db, url=connection_string)
  reports <- as.data.frame(reports$find())
}

# data = fetch_mongodb(connection_string, collection, db)

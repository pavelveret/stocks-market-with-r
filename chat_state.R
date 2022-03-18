library("DBI")

db_user <- ""  
db_password <- ""
db_name <- ""
db_host <- ""

kill_states <- function(chat_id) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("DELETE FROM chat_state where chat_id=%s",chat_id)
  try(
    dbGetQuery(con,sql)
  )
  dbDisconnect(con)
}

kill_portfolio_states <- function(chat_id) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("DELETE FROM add_portfile_state where chat_id=%s",chat_id)
  try(
    dbGetQuery(con,sql)
  )
  dbDisconnect(con)
}

get_portfolio_state <- function(chat_id) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT state FROM add_portfile_state WHERE chat_id = %s ORDER by date DESC LIMIT 1", chat_id)
  
  if (nrow(dbGetQuery(con,sql)) == 0) {
    dbDisconnect(con)
    return("start") 
  } else {
    chat_state <- dbGetQuery(con,sql)
    dbDisconnect(con)
    return(unlist(chat_state))
  }
}

# получить текущее состояние чата
get_state <- function(chat_id) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  
  
  sql <- sprintf("SELECT state FROM chat_state WHERE chat_id = %s ORDER by date DESC LIMIT 1", chat_id)
  
  if (nrow(dbGetQuery(con,sql)) == 0) {
    dbDisconnect(con)
    return("start") 
  } else {
    chat_state <- dbGetQuery(con,sql)
    dbDisconnect(con)
    return(unlist(chat_state))
  }
}

# установить текущее состояние чата
set_state <- function(chat_id, state) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  # upsert состояние чата
  kill_states(chat_id)
  sql <- sprintf("INSERT INTO chat_state (chat_id, state, date) VALUES(%s, \"%s\", \"%s\")", 
                 chat_id, state, Sys.time())
  dbGetQuery(con,sql)
  dbDisconnect(con)
}



# установить текущее состояние чата
set_portfolio_state <- function(chat_id, state) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  # upsert состояние чата
  kill_portfolio_states(chat_id)
  sql <- sprintf("INSERT INTO add_portfile_state (chat_id, state, date, func) VALUES(%s, \"%s\", \"%s\", \"%s\")", 
                 chat_id, state, Sys.time(), "add_portfolio")
  dbGetQuery(con,sql)
  dbDisconnect(con)
}

get_watchlist_state <- function(chat_id) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  
  sql <- sprintf("SELECT state FROM add_watchlist_state WHERE chat_id = %s ORDER by date DESC LIMIT 1", chat_id)
  
  if (nrow(dbGetQuery(con,sql)) == 0) {
    dbDisconnect(con)
    return("start") 
  } else {
    
    chat_state <- dbGetQuery(con,sql)
    
    dbDisconnect(con)
    return(unlist(chat_state))
  }
  
}

# установить текущее состояние чата
set_watchlist_state <- function(chat_id, state) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  # upsert состояние чата
  kill_watchlist_states(chat_id)
  sql <- sprintf("INSERT INTO add_watchlist_state (chat_id, state, date) VALUES(%s, \"%s\", \"%s\")", 
                 chat_id, state, Sys.time())
  
  dbGetQuery(con,sql)
  dbDisconnect(con)
}

kill_watchlist_states <- function(chat_id) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("DELETE FROM add_watchlist_state where chat_id=%s",chat_id)
  try(
    dbGetQuery(con,sql)
  )
  dbDisconnect(con)
}


get_temp_data <- function(chat_id, user_func="add_ticker") {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT field, value FROM temp_user WHERE chat_id = %s AND func = \"%s\"", chat_id, user_func)
  temp_ticker <- dbGetQuery(con,sql)
  temp_ticker <- transpose(temp_ticker)
  names(temp_ticker) <- as.character(unlist(temp_ticker[1,]))
  temp_ticker <- temp_ticker[2,]
  
  dbDisconnect(con)
  return(temp_ticker)
}

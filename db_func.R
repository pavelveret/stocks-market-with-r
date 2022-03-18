library("DBI")
library(quantmod)

db_user <- ""  
db_password <- ""
db_name <- ""
db_host <- ""

############# АДМИНСКИЕ ФУНКЦИИ #############

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
}

db_message_write <- function (chat_id="0", func="func", message="empty") {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("INSERT INTO messages_history (date, chat_id, func, message)
VALUES (\"%s\", %s,\"%s\",\"%s\")", Sys.time(),chat_id,func,message)
  dbGetQuery(con,sql)
  
  dbDisconnect(con)
}

############# end АДМИНСКИЕ ФУНКЦИИ #############

############# inline-add ФУНКЦИИ #############

inline_temp <- function(chat_id, func, field, value) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  sql <- sprintf("INSERT INTO temp_user (chat_id, func, field, value)
                  VALUES (%s, \"%s\",\"%s\",\"%s\")", 
                 chat_id, func, field, value)
  try(
    dbGetQuery(con,sql)
  )
  
  dbDisconnect(con)
}

empty_temp <- function(chat_id,user_func) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("DELETE FROM temp_user where chat_id=%s AND func=\"%s\"",chat_id, user_func)
  try(
    dbGetQuery(con,sql)
  )
  
  dbDisconnect(con)
}

portfolios_keyboard <- function(chat_id, pre_text) {
  try (
    portfolios <- get_portfolios(chat_id)
  )
  q<-nrow(portfolios)
  keys <- lapply(1:q, 
                 function(x) 
                   InlineKeyboardButton(portfolios$portfolio[x], 
                   callback_data = paste0(pre_text, portfolios$portfolio[x])))
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(keys,
    list(
      InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
     )
    )
  )
  
  return(IKM)
}

############# end inline-add ФУНКЦИИ #############

############# ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ #############
start_keyboard <- function(add_menu=list()) {
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      add_menu,
      list(
        InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
      )
    )
  )
  return(IKM)
}


quotes_table <- function(dframe) {
  
  dframe$Change_emoji <- ifelse(dframe$Change>0, "\xF0\x9F\x94\xBC", "\xF0\x9F\x94\xBB")
  
  dframe$Last_str <- round(dframe$Last,2)
 
  dframe$Change_str <- ifelse(dframe$Change>0, 
                             paste("+", round(dframe$Change,2), sep=""), 
                             round(dframe$Change,2))
  
  dframe$ps_Change_str <- ifelse(dframe$`% Change`>0, 
                                 paste("+", round(dframe$`% Change`,2), "%", sep=""), 
                                 paste(round(dframe$`% Change`,2), "%", sep=""))
  
  dframe$ps_Change_str_br <- paste("(",  dframe$ps_Change_str, ")", sep="")
  dframe$Change_str_br <- paste("(",  dframe$Change_str, ")", sep="")
  
  dframe
  
}

get_user_tickers <- function(chat_id, portfolio="default") {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT ticker, portfolio FROM user_tickers where chat_id=%s ORDER BY ticker",chat_id)
  user_tickers <- dbGetQuery(con,sql)
  dbDisconnect(con)
  
  return(user_tickers)
}

# check_ticker функция проверки наличия тикера в базе Yahoo Finance

check_ticker <- function(ticker) {
  tryCatch({
    t <- getQuote(ticker)
    t <- nrow(t)
    return(t)
  },
  error = function(e) {
    t <- 0 
    return(t)
  }
  )
}

# end check_ticker


# check_portfile_ticker функция проверки наличия тикера в базе пользователя

check_portfile_ticker <- function(chat_id, ticker, portfolio_name) {

  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT ticker FROM user_tickers where chat_id=%s 
                 AND ticker=\"%s\" 
                 AND portfolio=\"%s\"",
                 chat_id,ticker,portfolio_name)
  
  rows <- nrow(dbGetQuery(con,sql))
  dbDisconnect(con)
  
  return(rows)
  
}

check_user_portfolio <- function(chat_id, user_portfolio) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT portfolio FROM user_portfolios where chat_id=%s AND portfolio=\"%s\"",chat_id,user_portfolio)
  rows <- nrow(dbGetQuery(con,sql))
  
  dbDisconnect(con)
  return(rows)
}

user_portfolio_count <- function(chat_id) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT portfolio FROM user_portfolios where chat_id=%s",chat_id)
  rows <- nrow(dbGetQuery(con,sql))
  
  dbDisconnect(con)
  return(rows)
}
# end check_portfile_ticker


############# end ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ #############

############# ФУНКЦИИ БОТА #############
add_ticker_to_watchlist <- function (chat_id, ticker) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("INSERT INTO user_watchlist (chat_id, date, watch_ticker)
VALUES (%s, \"%s\", \"%s\")", chat_id, Sys.time(), ticker)
  
  dbGetQuery(con,sql)
  
  dbDisconnect(con)
}

check_watchlist_ticker <- function(chat_id, ticker) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT watch_ticker FROM user_watchlist where chat_id=%s 
                 AND watch_ticker=\"%s\"",
                 chat_id,ticker)
  
  rows <- nrow(dbGetQuery(con,sql))
  dbDisconnect(con)
  
  return(rows)
  
}

add_portfolio <- function (chat_id, portfolio_name) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("INSERT INTO user_portfolios (chat_id, date_added, portfolio)
VALUES (%s, \"%s\", \"%s\")", chat_id, Sys.time(), portfolio_name)
  
  dbGetQuery(con,sql)
  
  dbDisconnect(con)
}

get_portfolios <- function(chat_id) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT portfolio FROM user_portfolios WHERE chat_id = %s ORDER by portfolio", chat_id)
  portfolios <- dbGetQuery(con,sql)
  dbDisconnect(con)
  return(portfolios)
}

get_indexes <- function (){
  indexes <- c("^GSPC", "^DJI", "^IXIC", "^GDAXI", "^RUT")
  indexes_names <- c("S&P 500", "Dow Jones", "NASDAQ", "DAX", "Russell 2000")
  indexesDF <- data.frame(name=indexes_names)
  
  getIndexesDF <- getQuote(indexes, src = "yahoo")
  
  indexesDF <- cbind(indexesDF, quotes_table(getIndexesDF))
  
  indexesDF
}
get_watchlist <- function (chat_id){
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT watch_ticker FROM user_watchlist WHERE chat_id = %s ORDER by watch_ticker", chat_id)
  watchlist <- dbGetQuery(con,sql)
  dbDisconnect(con)
  
  
  
  watchlistDF <- data.frame(name=watchlist$watch_ticker)
  getWatchlist <- getQuote(watchlist$watch_ticker, src = "yahoo")
  watchlist_table <- cbind(watchlistDF, quotes_table(getWatchlist))
  
  return(watchlist_table)
}


db_add_ticker <- function (chat_id, ticker="0", price_buy=0, q=1) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
    
    price_buy <- as.numeric(gsub("\\,", ".", price_buy))
    sql <- sprintf("INSERT INTO user_tickers (date, chat_id, ticker, price_buy,quantity)
VALUES (\"%s\", %s, \"%s\", %s, %s)", Sys.time(),chat_id,ticker,price_buy,q)
    
    dbGetQuery(con,sql)

    dbDisconnect(con)
}


db_update_stocks <- function (chat_id,portfolio_name) {
  #chat_id<-10897773
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("SELECT chat_id,ticker,price_buy,quantity FROM user_tickers where chat_id=%s AND portfolio = \"%s\" order by ticker",chat_id, portfolio_name)
  try(
      user_tickers <- dbGetQuery(con,sql)
  )
  dbDisconnect(con)
  if (nrow(user_tickers) == 0){
    user_tickers <- 0
    user_tickers
  } else {
  
  tickers <- c(user_tickers$ticker)
  currentStock <- quotes_table(getQuote(tickers, src = "yahoo"))
  
  user_tickers$last <- currentStock$Last
  user_tickers$Last_str <- currentStock$Last_str
  user_tickers$Change_str_br <- currentStock$Change_str_br
  user_tickers$ps_Change_str_br <- currentStock$ps_Change_str_br
  user_tickers$today_diff <- round(currentStock$Change*user_tickers$quantity,2)
  user_tickers$Change_emoji <- currentStock$Change_emoji
  #user_tickers
  
  user_tickers$diff <- (user_tickers$last/user_tickers$price_buy) - 1
  user_tickers$emoji <- ifelse(user_tickers$diff>0, "\xF0\x9F\x94\xBC", "\xF0\x9F\x94\xBB")
  
  user_tickers$diff <- round(user_tickers$diff*100,2)
  user_tickers$diff <- ifelse(user_tickers$diff>0, 
                           paste("+", user_tickers$diff, sep=""), 
                           user_tickers$diff)
  
  user_tickers$diff <- paste(user_tickers$diff, "%", sep="")
  user_tickers$diff <- paste("(", user_tickers$diff, ")", sep="")
  
  user_tickers$total_now <- user_tickers$quantity*user_tickers$last
  user_tickers$total_buy <- user_tickers$quantity*user_tickers$price_buy
  
  user_tickers$total_diff <- round(user_tickers$total_now-user_tickers$total_buy,2)
  user_tickers$total_diff <- ifelse(user_tickers$total_diff>0, 
                                 paste("+", user_tickers$total_diff, sep=""), 
                                 user_tickers$total_diff)
  return(user_tickers)
  dbDisconnect(con)
  }
  
  
}

db_del_ticker <- function (chat_id, ticker, portfolio) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql <- sprintf("DELETE FROM user_tickers where chat_id=%s AND ticker=\"%s\" AND portfolio=\"%s\"",chat_id, ticker, portfolio)
  try(
    dbGetQuery(con,sql)
  )
  dbDisconnect(con)
}


db_edit_ticker <- function (chat_id, ticker="0", new_buy_price, new_q) {
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  new_buy_price <- as.numeric(gsub("\\,", ".", new_buy_price))
  
  sql <- sprintf("UPDATE user_tickers SET price_buy=%s,quantity=%s WHERE chat_id=%s AND ticker=\"%s\"", new_buy_price, new_q, chat_id, ticker)
  dbGetQuery(con,sql)
  
  dbDisconnect(con)
}

del_portfolio <- function(chat_id, portfolio_name) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql_del_portfolio <- sprintf("DELETE FROM user_portfolios where chat_id=%s AND portfolio=\"%s\"", chat_id, portfolio_name)
  try(
    dbGetQuery(con,sql_del_portfolio)
  )
  try(
    del_portfolio_tickers(chat_id, portfolio_name)
  )
  dbDisconnect(con)
}
del_portfolio_tickers <- function(chat_id, portfolio_name) {
  
  con = dbConnect(MySQL(), 
                  user=db_user, 
                  password=db_password, 
                  dbname=db_name, 
                  host=db_host)
  
  sql_del_tickers <- sprintf("DELETE FROM user_tickers where chat_id=%s AND portfolio=\"%s\"", chat_id, portfolio_name)
  try(
    dbGetQuery(con,sql_del_tickers)
  )
  dbDisconnect(con)
}

errorMessage <- function(chat_id){
  
}


############# end ФУНКЦИИ БОТА #############

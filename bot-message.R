library(telegram.bot)
library(httr)
library(quantmod)
library("DBI")
library(gridExtra)

source("db_func.R")
source("chat_state.R")
source("inline-add.R")

bot_token <- ""

e_warning <- "\xF0\x9F\x9A\xAB "
e_success <- "\xE2\x9C\x85 "
e_trash <- "\U0001F5D1 "

start <- function(bot, update) {
  db_message_write(update$from_chat_id(), "start")
  set_state(chat_id = update$from_chat_id(), state = 'start')
  set_portfolio_state(chat_id = update$from_chat_id(), state = 'start')
  set_watchlist_state(chat_id = update$from_chat_id(), state = 'start')
  
  message <- "What do you want me to do?"
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'My Watchlist', callback_data = 'check_watchlist'),
        InlineKeyboardButton(text = 'My Portfolio', callback_data = 'check_portfolio')
      ),
      list(
        InlineKeyboardButton(text = 'Check Indexes', callback_data = 'check_indexes'),
        InlineKeyboardButton(text = 'Add a Ticker', callback_data = 'add_choose')
      ),
      list(
        InlineKeyboardButton(text = 'Create Portfolio', callback_data = 'create_portfolio'),
        InlineKeyboardButton(text = 'Manage Portfolios', callback_data = 'more_functions')
  #      InlineKeyboardButton(text = '\xF0\x9F\x92\xB5 Donate', callback_data = 'donate')
      
    )
  )
)
  bot$sendMessage(
    chat_id = update$from_chat_id(),
    text = paste0(message, sep="", collapse=""),
    reply_markup = IKM
  )
}

add_choose <- function(bot, update) {
  db_message_write(update$from_chat_id(), "add_choose")
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Add to Watchlist', callback_data = 'add_watchlist')
      ),
      list(
        InlineKeyboardButton(text = 'Add to Portfolio', callback_data = 'ticker_add')
      )
    )
  )
  
  
  bot$sendMessage(
    chat_id = update$from_chat_id(),
    text = "Choose the option:",
    reply_markup = IKM
  )
}

answer_start <- function(bot, update) {
  data <- update$callback_query$data
  db_message_write(update$from_chat_id(), "answer_start", data)
  
  if (data =='check_indexes') {
    indexes(bot, update)
  } else if (data =='check_portfolio') {
    portfolio(bot, update)
  } else if (data =='create_portfolio') {
    new_portfolio(bot, update)
  } else if (data =='ticker_add') {
    ticker_add(bot, update)
  } else if (data =='more_functions') {
    more(bot, update)
  } else if (data =='add_watchlist') {
    add_watchlist(bot, update)
  } else if (data =='check_watchlist') {
    check_watchlist(bot, update)
  } else if (data =='add_choose') {
    add_choose(bot, update)
  } 
}

check_watchlist <- function(bot, update) {
  db_message_write(update$from_chat_id(), "check_watchlist")
  try (
    watchlist <- get_watchlist(update$from_chat_id())
  )
  
  pre_message <- "Your Watchlist by Yahoo Finance : \n"
  
  df_args <- list("<b>", watchlist$name,watchlist$Change_emoji, watchlist$ps_Change_str_br, "</b>\n",
                  "Last Price:", watchlist$Last_str,  watchlist$Change_str_br, "\n\n",
                  "========================\n"
  )
  
  data_message <- paste0(do.call(paste, df_args), sep="\n", collapse="")
  message <- paste0(c(pre_message, data_message),sep="\n", collapse="")
  
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Refresh Watchlist', callback_data = 'check_watchlist')
      ),
      list(
        InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
      )
    )
  )
  
  bot$sendMessage(
    chat_id = update$from_chat_id(),
    parse_mode = "HTML",
    text = message,
    reply_markup = IKM
  )
}

more <- function(bot, update) {
  db_message_write(update$from_chat_id(), "more")
  message <- "What do you want me to do?"
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Delete Portfolio', callback_data = 'delete_portfolio'),
        InlineKeyboardButton(text = 'Delete Ticker', callback_data = 'delete_ticker')
      ),
      list(
        InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
      )
    # list(
    #   InlineKeyboardButton(text = 'Create Portfolio', callback_data = 'create_portfolio'),
    #   InlineKeyboardButton(text = 'Add Ticker', callback_data = 'ticker_add')
    # )
      #    list(
      #      InlineKeyboardButton(text = 'Info', callback_data = 'bot_info'),
      #      InlineKeyboardButton(text = '\xF0\x9F\x92\xB5 Donate', callback_data = 'donate')
      #    
      #  )
    )
  )
  bot$sendMessage(
    chat_id = update$from_chat_id(),
    text = paste0(message, sep="", collapse=""),
    reply_markup = IKM
  )
}

answer_more <- function(bot, update) {
  data <- update$callback_query$data
  db_message_write(update$from_chat_id(), "answer_more", data)
  
  if (data =='delete_portfolio') {
    delete_portfolio(bot, update)
  } else if (data =='delete_ticker') {
    delete(bot, update)
  }
}

indexes <- function(bot, update) {
  db_message_write(update$from_chat_id(), "indexes")
  try (
    indexes_table <- get_indexes()
  )
  
  pre_message <- "Indexes by Yahoo Finance : \n"
  
  df_args <- list("<b>", indexes_table$name,indexes_table$Change_emoji, indexes_table$ps_Change_str_br, "</b>\n",
                  "Last Price:", indexes_table$Last_str,  indexes_table$Change_str_br, "\n\n",
                  "========================\n"
  )
  
  data_message <- paste0(do.call(paste, df_args), sep="\n", collapse="")
  message <- paste0(c(pre_message, data_message),sep="\n", collapse="")
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Refresh Indexes', callback_data = 'check_indexes')
      ),
      list(
        InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
      )
    )
  )
  
  bot$sendMessage(
    chat_id = update$from_chat_id(),
    parse_mode = "HTML",
    text = message,
    reply_markup = IKM
    )
}

answer_get_portfolio <- function(bot, update) {
  db_message_write(update$from_chat_id(), "answer_get_portfolio", update$callback_query$data)
  refresh_button_callback <- update$callback_query$data
  portfolio_name <- unlist(strsplit(refresh_button_callback, "[#]"))[2]
  
 tryCatch (
   my_update <- db_update_stocks(update$from_chat_id(), portfolio_name)
 )
 
 if (my_update == 0) {
   bot$sendMessage(
     chat_id = update$from_chat_id(),
     text = "Empty portfolio"
   )
 } else {
   
   dates_message <- paste("Your", portfolio_name, "portfolio:\n========================\n")
   
   df_args <- list("<b>", my_update$ticker, "</b>", my_update$Last_str, my_update$Change_str_br ,"\n\n",
                   
                   "Quantity: ", my_update$quantity,"\n",
                   "Sum: ", my_update$total_now,"\n\n",
                   
                   "<b>Today:</b>", my_update$today_diff, my_update$Change_emoji,my_update$ps_Change_str_br, "\n\n",
                   
                   "<b>Total:</b>", my_update$total_diff,my_update$emoji, my_update$diff, "\n\n",
                   "========================\n"
   )
   
   data_message <- paste0(do.call(paste, df_args), sep="\n", collapse="")
   message <- paste0(c(dates_message, data_message),sep="\n", collapse="")
   IKM <- InlineKeyboardMarkup(
     inline_keyboard = list(
       list(
         InlineKeyboardButton(text = 'Refresh Portfolio', callback_data = refresh_button_callback)
       ),
       list(
         InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
       )
     )
   )
   
   bot$sendMessage(
     chat_id = update$from_chat_id(),
     parse_mode = "HTML",
     text = message,
     reply_markup = IKM)
 }
}

portfolio <- function(bot, update) {
  db_message_write(update$from_chat_id(), "portfolio")
  portf_count <- user_portfolio_count(update$from_chat_id())
  if (portf_count == 0) {
    IKM <- start_keyboard()
    
    bot$sendMessage(update$from_chat_id(), 
                    text = "You have no portfolios yet. Please, create portfolio first",
                    reply_markup = IKM)
  } else {
    
    IKM <- portfolios_keyboard(update$from_chat_id(), "portfolio#")
    
    bot$sendMessage(
      chat_id = update$from_chat_id(),
      text = "Choose portfolio:",
      reply_markup = IKM
    )
  }
}

delete_portfolio <- function(bot, update) {
  db_message_write(update$from_chat_id(), "delete_portfolio")
  portf_count <- user_portfolio_count(update$from_chat_id())
  if (portf_count == 0) {
    IKM <- start_keyboard()
    
    bot$sendMessage(update$from_chat_id(), 
                    text = "You have no portfolios yet. Please, create portfolio first",
                    reply_markup = IKM)
  } else {
    IKM <- portfolios_keyboard(update$from_chat_id(), "portfolio_del#")
    
    bot$sendMessage(
      chat_id = update$from_chat_id(),
      text = "Choose portfolio to delete:",
      reply_markup = IKM
    )
  }
}

answer_delete_portfolio <- function(bot, update) {
  db_message_write(update$from_chat_id(), "answer_delete_portfolio")
  # полученные данные с кнопки
  data <- update$callback_query$data
  data <- unlist(strsplit(data, "[#]"))[2]
  db_message_write(update$from_chat_id(), "answer_delete_portfolio", update$callback_query$data)
  # обработка результата
  
  msg <- paste0(e_trash, data, " deleted")
  IKM <- start_keyboard()
  
  try(
    del_portfolio(update$from_chat_id(), data)
  )
  
  # Отправка сообщения
  bot$sendMessage(chat_id = update$from_chat_id(),
                  text = msg,
                  reply_markup = IKM
                  )
  
  # сообщаем боту, что запрос с кнопки принят
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}

delete <- function(bot, update) {
  db_message_write(update$from_chat_id(), "delete")
  try (
    tickers <- get_user_tickers(update$from_chat_id())
  )
  q<-nrow(tickers)
  keys <- lapply(1:q, function(x) list(InlineKeyboardButton(
                paste0(tickers$ticker[x], " (",tickers$portfolio[x], ")"),
                callback_data = paste0("ticker_del%", tickers$ticker[x],"%", tickers$portfolio[x]))))
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keys
  )
  
  bot$sendMessage(update$from_chat_id(), 
                  text = "What do you want to delete?",
                  reply_markup = IKM
  )
}

answer_del <- function(bot, update) {
  db_message_write(update$from_chat_id(), "answer_del", update$callback_query$data)
  # полученные данные с кнопки
  data <- update$callback_query$data
  data <- unlist(strsplit(data, "[%]"))
  data_ticker <- data[2]
  data_portfolio <- data[3]
  
  # обработка результата
  try(
    db_del_ticker(update$from_chat_id(), ticker=data_ticker, portfolio=data_portfolio)
  )
  msg <- paste0(e_trash, data_ticker, " deleted")
  
  IKM <- start_keyboard()
  
  # Отправка сообщения
  bot$sendMessage(chat_id = update$from_chat_id(),
                  text = msg,
                  reply_markup = IKM)
  
  # сообщаем боту, что запрос с кнопки принят
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}

updater <- Updater(bot_token) + CommandHandler("start", start)

updater <- updater + CommandHandler("more", more)
updater <- updater + CommandHandler("indexes", indexes)
updater <- updater + CommandHandler("portfolio", portfolio)
updater <- updater + CommandHandler("delete", delete)
updater <- updater + CommandHandler("delete_portfolio", delete_portfolio)

updater <- updater + CommandHandler("ticker_add", ticker_add)

updater <- updater + CommandHandler("add_watchlist", add_watchlist)

updater <- updater + CommandHandler("new_portfolio", new_portfolio)

updater <- updater + CallbackQueryHandler(answer_start, pattern = "check_indexes|check_portfolio|create_portfolio|ticker_add|more_functions|add_watchlist|check_watchlist|add_choose")
updater <- updater + CallbackQueryHandler(answer_more, pattern = "delete_portfolio|delete_ticker")
updater <- updater + CallbackQueryHandler(answer_del, pattern = "ticker_del%")
updater <- updater + CallbackQueryHandler(reset, pattern = "cancell_add_ticker")
updater <- updater + CallbackQueryHandler(cancell_add_watchlist, pattern = "cancel_watchlist")
updater <- updater + CallbackQueryHandler(answer_porfolio_select, pattern = "portfolio%")
updater <- updater + CallbackQueryHandler(answer_get_portfolio, pattern = "portfolio#")
updater <- updater + CallbackQueryHandler(answer_delete_portfolio, pattern = "portfolio_del#")
updater <- updater + CallbackQueryHandler(start, pattern = "back_to_start")

updater <- updater + CommandHandler("state", state)
updater <- updater + CommandHandler("reset", reset)

updater <- updater + MessageHandler(enter_ticker,  MessageFilters$wait_ticker  & !MessageFilters$command)
updater <- updater + MessageHandler(enter_quantity, MessageFilters$wait_quantity & !MessageFilters$command)
updater <- updater + MessageHandler(enter_price, MessageFilters$wait_price & !MessageFilters$command)

updater <- updater + MessageHandler(enter_portfolio_name, MessageFilters$wait_portfolio & !MessageFilters$command)

updater <- updater + MessageHandler(enter_watchlist_ticker, MessageFilters$wait_watchlist & !MessageFilters$command)

updater$start_polling() # Send "/start" to the bot

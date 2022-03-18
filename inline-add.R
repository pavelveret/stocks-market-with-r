add_watchlist <- function(bot, update) {
    db_message_write(update$from_chat_id(), "add_watchlist")
    set_watchlist_state(chat_id = update$from_chat_id(), state = 'start')
  
  
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(text = 'Cancel', callback_data = 'cancel_watchlist'))
      )
    )
    
    bot$sendMessage(update$from_chat_id(), 
                  text = "Please send me a ticker to add to your watchlist. For example: TSLA\n\nOr cancel operation:",
                  reply_markup = IKM)  
  
    set_watchlist_state(chat_id = update$from_chat_id(), state = 'wait_watchlist')
  
}
cancell_add_watchlist <- function(bot, update) {
  db_message_write(update$from_chat_id(), "cancell_add_watchlist")
  set_watchlist_state(chat_id = update$from_chat_id(), state = 'start')
  
  IKM <- start_keyboard()
  
  bot$sendMessage(update$from_chat_id(), 
                  text = "Operation canceled",
                  reply_markup = IKM)  
}

enter_watchlist_ticker <- function(bot, update) {
  db_message_write(update$from_chat_id(), "enter_watchlist_ticker",update$message$text)
  ticker <- paste0(update$message$text)
  
  #проверяем, есть ли тикер в Yahoo Finance
  check <- check_ticker(ticker)
  
  if (check == 0) { #начало проверки наличия тикеров
    
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_watchlist'))
      )
    )
    
    msg <- paste0(e_warning, "Error: Symbol not found in Yahoo Finance. Try again...\n\n Or cancel operation")
    bot$sendMessage(
      chat_id = update$message$chat$id,
      text = msg,
      reply_markup = IKM
    )
  } else {
    check_ticker_in_wl <- check_watchlist_ticker(update$message$chat_id, ticker)
    if (check_ticker_in_wl > 0) {
      msg <- paste0(e_warning, " Error: you already have this ticker in your watchlist.")
      
     IKM <- InlineKeyboardMarkup(
        inline_keyboard = list(
          list(
            InlineKeyboardButton(text = '<< Back to Start Menu', callback_data = 'back_to_start')
          )
        )
     )
      bot$sendMessage(update$message$chat_id, 
                      text = msg,
                      reply_markup = IKM)
    } else {
    
        msg <- paste0(e_success, "Good! You added ", ticker, " to your watchlist")
        # Отправляем пользователю сообщение, что все ок
        
        add_ticker_to_watchlist(update$message$chat_id, ticker)
        
        IKM <- start_keyboard()
        
        bot$sendMessage(update$message$chat_id, 
                        text = msg,
                        reply_markup = IKM)
        
        # Меняем состояние на ожидание ввода имени
        set_watchlist_state(chat_id = update$message$chat_id, state = 'start')
    }
  }
}

new_portfolio <- function(bot, update) {
  db_message_write(update$from_chat_id(), "new_portfolio")
  portf_count <- user_portfolio_count(update$from_chat_id())
  
  if(portf_count>4) {
    bot$sendMessage(update$from_chat_id(), 
                    text = "You can only create 5 portfolios"
    )
  } else {
  
  set_state(chat_id = update$from_chat_id(), state = 'start')
    
  bot$sendMessage(update$from_chat_id(), 
                  text = "Please send me a your new portfolio name. \n\n Only letters, \"-\" and \"_\" allowed.\n\n For example: Short_Time"
                  )
  set_portfolio_state(chat_id = update$from_chat_id(), state = 'wait_portfolio')
  }
}


enter_portfolio_name <- function(bot, update) {
  
  db_message_write(update$from_chat_id(), "enter_portfolio_name", paste0(update$message$text))
  set_portfolio_state(chat_id = update$message$chat_id, state = 'start')
  portfolio_name <- update$message$text
  portfolio_name <- paste0(portfolio_name)
 
  if(grepl("^[A-Za-z_-]+$", portfolio_name) == FALSE || nchar(portfolio_name)>15) {
    
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(text = 'Create Portfolio', callback_data = 'create_portfolio')
        ),
        list(
          InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
        )
      )
    )
    
    msg <- paste0(e_warning, "Error: Only letters, \"-\" and \"_\" allowed, 15 symbols max
                              \nPortfolio wasn't created")
    
    bot$sendMessage(
      chat_id = update$message$chat$id,
      text = msg,
      reply_markup = IKM
    )
    
  } else {
        check_portfolio <- check_user_portfolio(update$message$chat$id, portfolio_name)
        set_portfolio_state(chat_id = update$message$chat_id, state = 'start')
        if (check_portfolio > 0) {
          msg <- paste0(e_warning, "Error: You already have portfolio with that name. Try again...")
          bot$sendMessage(
            chat_id = update$message$chat$id,
            text = msg
          )
        } else {
          IKM <- InlineKeyboardMarkup(
            inline_keyboard = list(
              list(
                InlineKeyboardButton(text = 'Add Ticker', callback_data = 'ticker_add')
              ),
              list(
                InlineKeyboardButton(text = '<< Go to Start Menu', callback_data = 'back_to_start')
              )
            )
          )
          msg <- paste0(e_success, "All good. Now you can add tickers to you portfolio")
          
          bot$sendMessage(
            chat_id = update$message$chat$id,
            text = msg,
            reply_markup = IKM
            )
          add_portfolio(update$message$chat$id, portfolio_name)
          set_portfolio_state(update$message$chat$id, state = 'start')
        }
      }
  
}

ticker_add <- function(bot, update) {
  db_message_write(update$from_chat_id(), "ticker_add")
  empty_temp(update$from_chat_id(), "add_ticker")
  
  portf_count <- user_portfolio_count(update$from_chat_id())
  
  if (portf_count == 0) {
    IKM <- start_keyboard()
    
    bot$sendMessage(update$from_chat_id(), 
                    text = "You have no portfolios yet. Please, create portfolio first",
                    reply_markup = IKM)
  } else {
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_ticker'))
      )
    )
    
  bot$sendMessage(update$from_chat_id(), 
                  text = "Step 1/4\n\nPlease send me a ticker. For example: TSLA\n\nOr cancel operation:",
                  reply_markup = IKM)
  
  # переключаем состояние диалога в режим ожидания ввода имени
  set_state(chat_id = update$from_chat_id(), state = 'wait_ticker')
  }
}

state <- function(bot, update) {
  
  chat_state <- get_state(update$message$chat_id)
  
  # Send state
  bot$sendMessage(update$message$chat_id, 
                  text = unlist(chat_state))
  
}

reset <- function(bot, update) {
  
  IKM <- start_keyboard()
  
  bot$sendMessage(update$from_chat_id(), 
                  text = "Operation canceled",
                  reply_markup = IKM)
  
  set_state(chat_id = update$from_chat_id(), state = 'start')
  empty_temp(update$from_chat_id(), "add_ticker")
}

enter_ticker <- function(bot, update) {
  db_message_write(update$from_chat_id(), "enter_ticker", paste0(update$message$text))
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_ticker'))
    )
  )
  
  ticker <- paste0(update$message$text)
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_ticker'))
    )
  )
  #проверяем, есть ли тикер в Yahoo Finance
  check <- check_ticker(ticker)
  
  if (check == 0) { #начало проверки наличия тикеров
    msg <- paste0(e_warning, "Error: Symbol not found in Yahoo Finance. Try again...\n\n Or cancel operation")
    bot$sendMessage(
      chat_id = update$message$chat$id,
      text = msg,
      reply_markup = IKM
    )
  } else {
    inline_temp(update$message$chat$id, "add_ticker", "ticker", ticker)
    # Отправляем пользователю сообщение, что все ок
    bot$sendMessage(update$message$chat_id, 
                    text = paste0("Step 2/4\n\nNow, send me quantitty for ",ticker, "\n\nOr cancel operation"),
                    reply_markup = IKM)
    
    #set_chat_data(update$message$chat_id, 'name', uname) 
    
    # Меняем состояние на ожидание ввода имени
    set_state(chat_id = update$message$chat_id, state = 'wait_quantity')
  }
}

enter_quantity <- function(bot, update) {
  db_message_write(update$from_chat_id(), "enter_quantity", paste0(update$message$text))
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_ticker'))
    )
  )
  quantity <- as.numeric(update$message$text)
  
  # проверяем было введено число или нет
  if ( is.na(quantity) ) {
    
    msg <- paste0(e_warning, "Error: quantity must be a number. Try again:")
    # если введено не число то переспрашиваем возраст
    bot$sendMessage(update$message$chat_id, 
                    text = msg)
    
  } else {
    
    # если введено число сообщаем что возраст принят
    inline_temp(update$message$chat$id, "add_ticker", "quantity", quantity)
    
    bot$sendMessage(update$message$chat_id, 
                    text = paste0("Step 3/4\n\nNow, send me price\n\nOr cancel operation"),
                    reply_markup = IKM)
    
    # сообщаем какие данные были собраны
    #username <- get_chat_data(update$message$chat_id, 'name')
    #userage  <- get_chat_data(update$message$chat_id, 'age')
    
    # переводим диалог в состояние wait_price
    set_state(chat_id = update$message$chat_id, state = 'wait_price')
  }
  
}

enter_price <- function(bot, update) {
  db_message_write(update$from_chat_id(), "enter_quantity", paste0(update$message$text))
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Cancel', callback_data = 'cancell_add_ticker'))
    )
  )
  price <- as.numeric(gsub("\\,", ".", update$message$text))
  
  # проверяем было введено число или нет
  if ( is.na(price)) {
    
    # если введено не число то переспрашиваем возраст
    bot$sendMessage(update$message$chat_id, 
                    text = "Error: price must be a number. Try again:")
    
  } else {
    inline_temp(update$message$chat$id, "add_ticker", "price", price)
    
    # возвращаем диалог в исходное состояние
    set_state(chat_id = update$message$chat_id, state = 'wait_portfolio_select')
  }
  
  IKM <- portfolios_keyboard(update$message$chat$id, "portfolio%")
  
  
  temp_data <- get_temp_data(update$message$chat_id) 
  message <- list("Step 4/4\n\nYou are trying to add ", temp_data$ticker,"x", temp_data$quantity,  " bought for " , temp_data$price, "\n\n", "Select portfolio:")
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = paste0(message, sep="", collapse=""),
    reply_markup = IKM
  )
}
answer_porfolio_select <- function(bot, update) {
  db_message_write(update$from_chat_id(), "answer_porfolio_select", paste0(update$callback_query$data))
  portfolio_name <- update$callback_query$data
  portfolio_name <- unlist(strsplit(portfolio_name, "[%]"))[2]
  temp_data <- get_temp_data(update$from_chat_id())
  
  check_ticker_in_portfolio <- 
    check_portfile_ticker(update$from_chat_id(), temp_data$ticker, portfolio_name)
  
  if (check_ticker_in_portfolio >0) {
    IKM <- portfolios_keyboard(update$from_chat_id(), "portfolio%")
    
    msg <- paste0(e_warning, "Error: this ticker is already added to \"", 
                  portfolio_name, "\" portfolio.\n\nPlease, select another one:")
    bot$sendMessage(
      chat_id = update$from_chat_id(),
      text = msg,
      reply_markup = IKM
    )
    
  } else {
  
    con = dbConnect(MySQL(), 
                    user='user', 
                    password='xElA9AUFM__R1jdjsO', 
                    dbname='stocks-bot', 
                    host='localhost')
    
    sql <- sprintf("INSERT INTO user_tickers (chat_id, date, ticker, 	price_buy, quantity, portfolio) 
                   VALUES (%s, \"%s\", \"%s\",%s, %s,\"%s\")", 
                   update$from_chat_id(), Sys.time(), temp_data$ticker, temp_data$price, temp_data$quantity, portfolio_name)
    dbGetQuery(con,sql)
    dbDisconnect(con)
    
    empty_temp(update$from_chat_id(), "add_ticker")
    set_state(chat_id = update$from_chat_id(), state = 'start')
    
    message_success <- list(e_success, "Good! You added ", temp_data$ticker,"x", temp_data$quantity,
                    " bought for " , temp_data$price, 
                    " in your ", portfolio_name,  " portfolio")
    
    IKM <- start_keyboard(
        list(
          InlineKeyboardButton(text = 'Add Another Ticker', callback_data = 'ticker_add')
        )
    )
      
    
    bot$sendMessage( 
      chat_id = update$from_chat_id(),
      text = paste0(message_success, sep="", collapse=""),
      reply_markup = IKM
    )
  }
  
}

answer_cancel <- function(bot, update) {
  db_message_write(update$from_chat_id(), "answer_cancel")
  data <- update$callback_query$data
    reset()
  # bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}


MessageFilters$wait_ticker <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_ticker"
}
)

# фильтр сообщений в состоянии ожидания возраста
MessageFilters$wait_quantity <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_quantity"
}
)

MessageFilters$wait_price <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_price"
}
)

MessageFilters$wait_portfolio <- BaseFilter(function(message) {
  get_portfolio_state( message$chat_id )  == "wait_portfolio"
}
)



MessageFilters$wait_watchlist <- BaseFilter(function(message) {
  get_watchlist_state( message$chat_id )  == "wait_watchlist"
}
)

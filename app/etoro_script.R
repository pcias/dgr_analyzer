library(rio)
#library(writexl)
library(urltools)
library(zoo)
library(xts)
library(tidyr)
library(dplyr)
library(lubridate)
#library(alphavantager)
#library(ggplot2)
#library(plotly)
library(tidyquant)
#library(PerformanceAnalytics)
#library(knitr)
library(reactable)
library(data.table)









################################


read_etoro_closed_pos <- function(filename, from_date, to_date) {

  
  ret <- read_excel(filename, sheet = "Closed Positions", col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", 
                                                                           "numeric", "text", "text", "numeric", 
                                                                           "numeric", "numeric", "text", "numeric", 
                                                                           "numeric"))
  
  
  names(ret) <- c("positionid", "action", "copy_trader_name", "amount", "units","open_rate","close_rate", "spread", "profit", "open_date_time" , 
                  "close_date_time", "take_profit_rate" ,"stop_loss_rate", "fees_and_dividends", "is_real", "leverage", "notes")
  
  ret<-ret%>%mutate(open_date = as.Date.character(substr(open_date_time,1,10),"%d/%m/%Y"), close_date = as.Date.character(substr(close_date_time,1,10),"%d/%m/%Y"), profit_minus_fees = profit + fees_and_dividends, currency="USD", leverage =if_else(is.na(leverage),1,leverage))
  
  
  
  nbp<-read_nbp(tibble(quotecur=c("USD")),"2016-01-01","2020-12-31")
  
  ret<-ret%>%left_join(nbp %>% rename_with(function(x) { paste(x, "_nbp_open", sep="")}), by=c("currency" = "curcode_nbp_open", "open_date" = "effectiveDate_nbp_open"))
  ret<-ret%>%left_join(nbp %>% rename_with(function(x) { paste(x, "_nbp_close", sep="")}), by=c("currency" = "curcode_nbp_close", "close_date" = "effectiveDate_nbp_close")) 
  ret<-ret%>%mutate(open_rate_PLN = open_rate * mid_nbp_open, close_rate_PLN = close_rate*mid_nbp_close, fees_and_dividends_PLN = fees_and_dividends * mid_nbp_close, profit_PLN = (close_rate_PLN - open_rate_PLN) * units / leverage, profit_minus_fees_PLN = profit_PLN + fees_and_dividends_PLN)
  
  
  return(ret)  
}




#########################


#DATA LOAD




# pSBreactable <- function(psbmap) {
#   openlongpos <- psbmap%>% filter(is.na(repSells_transactionid)) %>% select(Tbuys_isin, Tbuys_product, repBuys_transactionid, qty,Tbuys_date,Tbuys_time,Tbuys_quote,Tbuys_quotecur,Tbuys_mid_nbptables_cur,Tbuys_valuePLN_portion,Tbuys_feePLN_portion)
#   openshortpos <-psbmap%>% filter(is.na(repBuys_transactionid)) %>% select(Tsells_isin, repSells_transactionid, qty,Tsells_date,Tsells_time,Tsells_quote,Tsells_quotecur,Tsells_mid_nbptables_cur,Tsells_valuePLN_portion,Tsells_feePLN_portion)
#   closedpos <- psbmap%>% filter(!is.na(repBuys_transactionid) , !is.na(repSells_transactionid)) %>% 
#       select(Tbuys_isin, Tbuys_product, repBuys_transactionid, qty,Tbuys_date,Tbuys_time,Tbuys_quote,Tbuys_quotecur,Tbuys_mid_nbptables_cur,Tbuys_valuePLN_portion,Tbuys_feePLN_portion,
#             repSells_transactionid, qty,Tsells_date,Tsells_time,Tbuys_quote,Tsells_quotecur,Tsells_mid_nbptables_cur,Tsells_valuePLN_portion,Tsells_feePLN_portion,closing_year,gainsPLN,gainsMinusFeesPLN)
#                   
#   
#   react_openlongpos <- reactable(
#     openlongpos,
#     groupBy = "Tbuys_isin",
#     defaultExpanded = TRUE,
#     pagination = FALSE,
#     compact = TRUE,
#     striped = TRUE,
#     bordered = TRUE,
#     highlight = TRUE,
#     resizable = TRUE,
#     defaultColDef = colDef(
#       minWidth = 10,
#       format = colFormat(digits = 2, separators = TRUE),
#       style = list(fontSize = "small")
#     ),
#     columns = list(
#       Tbuys_isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
#       Tbuys_product = colDef(name="Produkt"),
#       repBuys_transactionid = colDef(name = "TxID zakupu"),
#       qty = colDef(name = "ilość",aggregate = "sum"),
#       Tbuys_date = colDef(name="data otw"),
#       Tbuys_time = colDef(name="czas otw"),
#       Tbuys_quote = colDef(name="cena zakupu", format=colFormat(separators = TRUE)),
#       Tbuys_quotecur = colDef(name="waluta"),
#       Tbuys_mid_nbptables_cur = colDef(name = "kurs nbp zakupu", format=colFormat(separators = TRUE)),
#       Tbuys_valuePLN_portion = colDef(name="Koszt pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       Tbuys_feePLN_portion = colDef(name="opłaty zak", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))                               
#       )
#   )
#   
#   react_openshortpos <- reactable(
#     openshortpos,
#     groupBy = "Tsells_isin",
#     defaultExpanded = TRUE,
#     pagination = FALSE,
#     compact = TRUE,
#     striped = TRUE,
#     bordered = TRUE,
#     highlight = TRUE,
#     resizable = TRUE,
#     defaultColDef = colDef(
#       minWidth = 10,
#       format = colFormat(digits = 2, separators = TRUE),
#       style = list(fontSize = "small")
#     ),
#     columns = list(
#       Tsells_isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
#       Tsells_product = colDef(name="Produkt"),
#       repSells_transactionid = colDef(name = "TxID sprzedaży"),
#       qty = colDef(name = "ilość",aggregate = "sum"),
#       Tsells_date = colDef(name="data otw"),
#       Tsells_time = colDef(name="czas otw"),
#       Tsells_quote = colDef(name="cena sprzedaży", format=colFormat(separators = TRUE)),
#       Tsells_quotecur = colDef(name="waluta"),
#       Tsells_mid_nbptables_cur = colDef(name = "kurs nbp sprzedaży", format=colFormat(separators = TRUE)),
#       Tsells_valuePLN_portion = colDef(name="Przychód pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       Tsells_feePLN_portion = colDef(name="opłaty sprz", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))                              
#     )
#   )
#   
#   
#   react_closedpos <- reactable(
#     closedpos,
#     groupBy = c("closing_year","Tbuys_isin"),
#     defaultExpanded = TRUE,
#     pagination = FALSE,
#     compact = TRUE,
#     striped = TRUE,
#     bordered = TRUE,
#     highlight = TRUE,
#     resizable = TRUE,
#     defaultColDef = colDef(
#       minWidth = 10,
#       format = colFormat(digits = 2, separators = TRUE),
#       style = list(fontSize = "small")
#     ),
#     columns = list(
#       Tbuys_isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
#       closing_year = colDef(name="rok zamknięcia"),
#       Tbuys_product = colDef(name="Produkt"),
#       repBuys_transactionid = colDef(name = "TxID zakupu"),
#       qty = colDef(name = "ilość",aggregate = "sum"),
#       Tbuys_date = colDef(name="data zak"),
#       Tbuys_time = colDef(name="czas zak"),
#       Tbuys_quote = colDef(name="cena zakupu", format=colFormat(separators = TRUE)),
#       Tbuys_quotecur = colDef(name="waluta"),
#       Tbuys_mid_nbptables_cur = colDef(name = "kurs nbp zakupu", format=colFormat(separators = TRUE)),
#       Tbuys_valuePLN_portion = colDef(name="Koszt pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       Tbuys_feePLN_portion = colDef(name="opłaty zak", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       repSells_transactionid = colDef(name = "TxID sprzedaży"),
#       qty = colDef(name = "ilość",aggregate = "sum"),
#       Tsells_date = colDef(name="data sprz"),
#       Tsells_time = colDef(name="czas sprz"),
#       Tsells_quote = colDef(name="cena sprzedaży", format=colFormat(separators = TRUE)),
#       Tsells_quotecur = colDef(name="waluta"),
#       Tsells_mid_nbptables_cur = colDef(name = "kurs nbp sprzedaży", format=colFormat(separators = TRUE)),
#       Tsells_valuePLN_portion = colDef(name="Przychód pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       Tsells_feePLN_portion = colDef(name="opłaty sprz", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       gainsPLN = colDef(name="Dochód", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
#       gainsMinusFeesPLN = colDef(name="Dochód po opłatach", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))
#     )
#   )
#   
#   
#   return(list(long=react_openlongpos, short=react_openshortpos, closed=react_closedpos))
# }
# 

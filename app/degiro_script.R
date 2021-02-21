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

#library(svDialogs)

library(rnbp)
#library(fifovaluation)



source('cbind.na.R')



################################


read_transactions <- function(sample_url, from_date, to_date) {
  data_url <- sample_url
  
  path(data_url) <- "reporting/secure/v3/transactionReport/csv"
  from_date_format<-format(as.Date(from_date), format="%d/%m/%Y")
  to_date_format<-format(as.Date(to_date), format="%d/%m/%Y")
  data_url<-param_set(data_url,"fromDate", url_encode(from_date_format))
  data_url<-param_set(data_url,"toDate", url_encode(to_date_format))
  
  ret <- import(data_url, format = "csv", 
                #col_types = c("text", "text", "text","text", "text","text", "text", "text","text","text", "text", "text", "text", "text","text","text","text","text","text"), 
                #col_names =  c("date", "time", "product", "isin", "market","execution","qty", "quote", "quotecur", "locval" , 
                #               "loccur", "value" ,"valuecur", "fxrate", "fee", "feecur", "total","totalcur", "orderid"), 
                skip=1)
  
  names(ret) <- c("date", "time", "product", "isin", "market","execution","qty", "quote", "quotecur", "locval" , 
                  "loccur", "value" ,"valuecur", "fxrate", "fee", "feecur", "total","totalcur", "orderid")
  
  ret<-ret%>%group_by(orderid)%>%mutate(transactionid = paste(orderid, row_number(), sep="-"))%>%ungroup()
  
  #clean non-breaking spaces in excel numericals 
  numeric_cols <- c("qty", "quote", "locval", "value", "fxrate", "fee", "total")
  
  #degiro bug workarounds
  ret$product <- substr(ret$product, 1, 34)
  ret <- ret%>%filter(!is.na(qty))
  
  
  # for(col in numeric_cols) {
  #   print(col)
  #   ret[[col]] <- (gsub("\u00A0","", ret[[col]]))
  #   ret[[col]] <- as.numeric(gsub(",",".", ret[[col]]))
  # }  
  ret$date <- as.Date.character(ret$date, "%d-%m-%Y")   
  
  return(ret)  
}


read_nbp <- function(currencies, analysis_start, analysis_end) {
  #need to go by years as nbp api maxes out at 367 days
  year_start <- substr(analysis_start,1,4)
  year_end <- substr(analysis_end,1,4)
  forex <- tibble()
  
  for(curcode in (currencies%>%filter(quotecur!="PLN"))$quotecur) {
    #quick hack
    if(curcode=="GBX") {curcode <- "GBP"}
    
    for(y in year_start:year_end) {
      date_start = as_date(paste(y,"-01-01"))
      date_end = as_date(paste(y,"-12-31"))
      if(date_end > today()) { date_end <- today()}
      f<-get_exchangerate_from_interval("A",curcode, date_start, date_end)$content$rates
      f<-f %>% complete(effectiveDate = seq.Date(as.Date(date_start), as.Date(date_end), by="day")) %>% fill(no,mid)
      f<-cbind(curcode,f)
      forex<-forex %>% bind_rows(f)
    }
  }
  
  #GBX now : create from GBP rows
  forex <- forex%>%bind_rows(forex%>%filter(curcode=="GBP")%>%mutate(curcode="GBX", mid=mid/1000))
  #for simplicity add PLN rows for all dates
  forex<-forex%>%bind_rows(forex%>%filter(curcode=="EUR")%>%mutate(curcode="PLN", mid=1))
  
  return(forex)
}


#########################


#DATA LOAD





########################################################
#MAPPING BUYS - SELLS

positionsSellBuyMap <- function(p_Transactions, p_isin) {
  Tsells <-
    p_Transactions %>%  ungroup () %>% filter(isin == p_isin) %>% filter(qty < 0) %>% arrange(date, time) 
  Tbuys <-
    p_Transactions  %>% ungroup () %>% filter(isin == p_isin) %>% filter(qty > 0) %>% arrange(date, time) 
  
  
  repBuys <- Tbuys  %>% summarise(transactionid = rep(transactionid, abs(qty), qty = 1))
  repSells <- Tsells %>% summarise(transactionid = rep(transactionid, abs(qty), qty = 1))
  
  names(repBuys) <- paste("repBuys",names(repBuys),sep=".")
  names(repSells) <- paste("repSells",names(repSells),sep=".")
  #if(nrow(repSells) == 0 ) {repSells <- repSells%>% ungroup() %>% add_row()}  
  #if(nrow(repBuys) == 0 ) {repBuys <- repSells%>% ungroup() %>% add_row()}  
  
  lineupSellBuy <- cbind.na(repBuys,repSells)
  #    as_tibble(cbind(repSells, repBuys)) %>% mutate(qty = 1)
  
  ret <-
    
    lineupSellBuy %>% group_by(repBuys.transactionid, repSells.transactionid) %>% summarise(qty = n(), .groups="keep") 
  
  return(ret)
}




pSBmap <- function(sample_url) {
  
  analysis_start <-"2016-12-05"         
  analysis_end <- as.character(Sys.Date())
  dates <- DATE_SEQUENCE(analysis_start, analysis_end)
  
  
  Transactions <- read_transactions(sample_url, analysis_start, analysis_end)
  
  currencies <- Transactions%>%select(quotecur)%>%bind_rows(list(quotecur="EUR"))%>%distinct(quotecur)
  isins <- Transactions%>%select(isin)%>%distinct(isin)
  
  nbptables <- read_nbp(currencies, analysis_start,analysis_end)
  
  
  #now join Transations with forex to valuate transaction
  Transactions <- Transactions%>%left_join(nbptables %>% rename_with(function(x) { paste(x, ".nbptables.cur", sep="")}), by = c("date" = "effectiveDate.nbptables.cur", "quotecur" = "curcode.nbptables.cur"))
  
  #and join with EUR/PLN forex to valuate fees
  Transactions <- Transactions %>% left_join(nbptables %>% rename_with(function(x) { paste(x, ".nbptables.eur", sep="")}) %>% filter(curcode.nbptables.eur=="EUR"), by = c("date" = "effectiveDate.nbptables.eur"))
  
  
  #valuate transaction and fees in PLN
  Transactions <- Transactions%>%mutate(valuePLN = locval*mid.nbptables.cur, feePLN= fee*mid.nbptables.eur, quotePLN = quote*mid.nbptables.cur)
  
  
  #Transactions_prefixed<-Transactions
  ret <- tibble()
  for(i in isins$isin) {
    #print(i)
    #print(positionsSellBuyMap(i))
    
    #slower
    #ret <- bind_rows(ret, positionsSellBuyMap(Transactions,i))
    
    #faster
    ret <- rbindlist(list(ret,positionsSellBuyMap(Transactions,i)))
  }
  
  Tsells <- Transactions %>% filter(qty<0)
  Tbuys <- Transactions %>% filter(qty>0)
  names(Tsells) <- paste("Tsells",names(Tsells),sep=".")
  names(Tbuys) <- paste("Tbuys",names(Tbuys),sep=".")
  
  ret <- ret %>% left_join(Tbuys, by = c("repBuys.transactionid" = "Tbuys.transactionid")) %>% left_join(Tsells, by = c("repSells.transactionid" = "Tsells.transactionid")) %>% arrange(Tbuys.isin,Tbuys.date, Tbuys.time)
  
  ##### gains and fees
  ret <- ret %>% mutate( Tsells.portion = -qty/Tsells.qty, Tbuys.portion = qty/Tbuys.qty, Tbuys.feePLN_portion = Tbuys.feePLN * Tbuys.portion, Tsells.feePLN_portion = Tsells.fee * Tsells.portion, 
                         Tbuys.valuePLN_portion = Tbuys.valuePLN*Tbuys.portion , Tsells.valuePLN_portion = Tsells.valuePLN*Tsells.portion, gainsPLN = Tsells.valuePLN_portion + Tbuys.valuePLN_portion, 
                         gainsMinusFeesPLN = gainsPLN+Tsells.feePLN_portion+Tbuys.feePLN_portion,
                         closing_date = as.Date(ifelse(Tbuys.date > Tsells.date, Tbuys.date, Tsells.date)), closing_year = year(as.Date(closing_date)))
  
  return(ret)  
}


openlongpos <- function(psbmap) {
  return(psbmap%>% filter(is.na(repSells.transactionid)) %>% select(Tbuys.isin, Tbuys.product, repBuys.transactionid, qty,Tbuys.date,Tbuys.time,Tbuys.quote,Tbuys.quotecur,Tbuys.mid.nbptables.cur,Tbuys.valuePLN_portion,Tbuys.feePLN_portion))
}

openshortpos <- function(psbmap) {
  return(psbmap%>% filter(is.na(repBuys.transactionid)) %>% select(Tsells.isin, repSells.transactionid, qty,Tsells.date,Tsells.time,Tsells.quote,Tsells.quotecur,Tsells.mid.nbptables.cur,Tsells.valuePLN_portion,Tsells.feePLN_portion))
}

closedpos <- function(psbmap) {
  return(psbmap%>% filter(!is.na(repBuys.transactionid) , !is.na(repSells.transactionid)) %>% 
    select(Tbuys.isin, Tbuys.product, repBuys.transactionid, qty,Tbuys.date,Tbuys.time,Tbuys.quote,Tbuys.quotecur,Tbuys.mid.nbptables.cur,Tbuys.valuePLN_portion,Tbuys.feePLN_portion,
           repSells.transactionid, qty,Tsells.date,Tsells.time,Tbuys.quote,Tsells.quotecur,Tsells.mid.nbptables.cur,Tsells.valuePLN_portion,Tsells.feePLN_portion,closing_year,gainsPLN,gainsMinusFeesPLN))
}


pSBreactable <- function(psbmap) {
  openlongpos <- psbmap%>% filter(is.na(repSells.transactionid)) %>% select(Tbuys.isin, Tbuys.product, repBuys.transactionid, qty,Tbuys.date,Tbuys.time,Tbuys.quote,Tbuys.quotecur,Tbuys.mid.nbptables.cur,Tbuys.valuePLN_portion,Tbuys.feePLN_portion)
  openshortpos <-psbmap%>% filter(is.na(repBuys.transactionid)) %>% select(Tsells.isin, repSells.transactionid, qty,Tsells.date,Tsells.time,Tsells.quote,Tsells.quotecur,Tsells.mid.nbptables.cur,Tsells.valuePLN_portion,Tsells.feePLN_portion)
  closedpos <- psbmap%>% filter(!is.na(repBuys.transactionid) , !is.na(repSells.transactionid)) %>% 
      select(Tbuys.isin, Tbuys.product, repBuys.transactionid, qty,Tbuys.date,Tbuys.time,Tbuys.quote,Tbuys.quotecur,Tbuys.mid.nbptables.cur,Tbuys.valuePLN_portion,Tbuys.feePLN_portion,
            repSells.transactionid, qty,Tsells.date,Tsells.time,Tbuys.quote,Tsells.quotecur,Tsells.mid.nbptables.cur,Tsells.valuePLN_portion,Tsells.feePLN_portion,closing_year,gainsPLN,gainsMinusFeesPLN)
                  
  
  react_openlongpos <- reactable(
    openlongpos,
    groupBy = "Tbuys.isin",
    defaultExpanded = TRUE,
    pagination = FALSE,
    compact = TRUE,
    striped = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    defaultColDef = colDef(
      minWidth = 10,
      format = colFormat(digits = 2, separators = TRUE),
      style = list(fontSize = "small")
    ),
    columns = list(
      Tbuys.isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
      Tbuys.product = colDef(name="Produkt"),
      repBuys.transactionid = colDef(name = "TxID zakupu"),
      qty = colDef(name = "ilość",aggregate = "sum"),
      Tbuys.date = colDef(name="data otw"),
      Tbuys.time = colDef(name="czas otw"),
      Tbuys.quote = colDef(name="cena zakupu", format=colFormat(separators = TRUE)),
      Tbuys.quotecur = colDef(name="waluta"),
      Tbuys.mid.nbptables.cur = colDef(name = "kurs nbp zakupu", format=colFormat(separators = TRUE)),
      Tbuys.valuePLN_portion = colDef(name="Koszt pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      Tbuys.feePLN_portion = colDef(name="opłaty zak", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))                               
      )
  )
  
  react_openshortpos <- reactable(
    openshortpos,
    groupBy = "Tsells.isin",
    defaultExpanded = TRUE,
    pagination = FALSE,
    compact = TRUE,
    striped = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    defaultColDef = colDef(
      minWidth = 10,
      format = colFormat(digits = 2, separators = TRUE),
      style = list(fontSize = "small")
    ),
    columns = list(
      Tsells.isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
      Tsells.product = colDef(name="Produkt"),
      repSells.transactionid = colDef(name = "TxID sprzedaży"),
      qty = colDef(name = "ilość",aggregate = "sum"),
      Tsells.date = colDef(name="data otw"),
      Tsells.time = colDef(name="czas otw"),
      Tsells.quote = colDef(name="cena sprzedaży", format=colFormat(separators = TRUE)),
      Tsells.quotecur = colDef(name="waluta"),
      Tsells.mid.nbptables.cur = colDef(name = "kurs nbp sprzedaży", format=colFormat(separators = TRUE)),
      Tsells.valuePLN_portion = colDef(name="Przychód pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      Tsells.feePLN_portion = colDef(name="opłaty sprz", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))                              
    )
  )
  
  
  react_closedpos <- reactable(
    closedpos,
    groupBy = c("closing_year","Tbuys.isin"),
    defaultExpanded = TRUE,
    pagination = FALSE,
    compact = TRUE,
    striped = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    defaultColDef = colDef(
      minWidth = 10,
      format = colFormat(digits = 2, separators = TRUE),
      style = list(fontSize = "small")
    ),
    columns = list(
      Tbuys.isin = colDef(style = list(fontWeight = 600, fontSize = "small"), name = "ISIN"),
      closing_year = colDef(name="rok zamknięcia"),
      Tbuys.product = colDef(name="Produkt"),
      repBuys.transactionid = colDef(name = "TxID zakupu"),
      qty = colDef(name = "ilość",aggregate = "sum"),
      Tbuys.date = colDef(name="data zak"),
      Tbuys.time = colDef(name="czas zak"),
      Tbuys.quote = colDef(name="cena zakupu", format=colFormat(separators = TRUE)),
      Tbuys.quotecur = colDef(name="waluta"),
      Tbuys.mid.nbptables.cur = colDef(name = "kurs nbp zakupu", format=colFormat(separators = TRUE)),
      Tbuys.valuePLN_portion = colDef(name="Koszt pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      Tbuys.feePLN_portion = colDef(name="opłaty zak", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      repSells.transactionid = colDef(name = "TxID sprzedaży"),
      qty = colDef(name = "ilość",aggregate = "sum"),
      Tsells.date = colDef(name="data sprz"),
      Tsells.time = colDef(name="czas sprz"),
      Tsells.quote = colDef(name="cena sprzedaży", format=colFormat(separators = TRUE)),
      Tsells.quotecur = colDef(name="waluta"),
      Tsells.mid.nbptables.cur = colDef(name = "kurs nbp sprzedaży", format=colFormat(separators = TRUE)),
      Tsells.valuePLN_portion = colDef(name="Przychód pozycji", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      Tsells.feePLN_portion = colDef(name="opłaty sprz", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      gainsPLN = colDef(name="Dochód", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE)),
      gainsMinusFeesPLN = colDef(name="Dochód po opłatach", aggregate = "sum", format = colFormat(currency = "PLN", separators = TRUE))
    )
  )
  
  
  return(list(long=react_openlongpos, short=react_openshortpos, closed=react_closedpos))
}

test<-function() {
  
  sample_url <- "https://trader.degiro.nl/reporting/secure/v3/positionReport/xls?intAccount=71001415&sessionId=0769C5C351A301DA79055003AB6DDD3D.prod_b_126_1&country=PL&lang=pl&toDate=18%2F02%2F2021"
  #sample_url <- dlg_input("Paste here portfolio download xls link (portfolio->download->XLS):", default = sample_url)$res
  
  psb <- pSBmap(sample_url)
  
  output <- pSBreactable(psb)
  output$closed
}

test_fakeit<-function() {
  psb <- readRDS("fakeit.rds")
  output <- pSBreactable(psb)
  output$closed
}
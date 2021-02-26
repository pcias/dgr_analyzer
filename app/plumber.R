#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

.libPaths( c( .libPaths(), "~/Rlibs") )

source("degiro_script.R")

#library(plumber)

#* @apiTitle Plumber Example API



#* provide summary for closed positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /closed_positions
#* @serializer htmlwidget
function(sample_url) {
  psb <- pSBmap(sample_url)
  output <- pSBreactable(psb)
  
  return(output$closed)
}


#* provide summary of long positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /long_positions
#* @serializer htmlwidget
function(sample_url) {
  psb <- pSBmap(sample_url)
  output <- pSBreactable(psb)
  
  return(output$long)
}

#* provide summary of short positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /short_positions
#* @serializer htmlwidget
function(sample_url) {
  psb <- pSBmap(sample_url)
  output <- pSBreactable(psb)
  
  return(output$short)
}


#* fake (testing) summary for closed positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /fakeit
#* @serializer htmlwidget
function(sample_url) {
  psb <- readRDS("fakeit.rds")
  output <- pSBreactable(psb)
  
  #return(psb)
  return(output$closed)
}

#* fake (testing) summary for closed positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /fakeit_json
#* @serializer json
function(sample_url) {
  psb <- readRDS("fakeit.rds")
  return(psb)
  
}


#* provide summary for closed positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /closed_positions_json
#* @serializer json
function(sample_url) {
  if(sample_url=="fakeit") {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb<-"brak danych"
    psb <- pSBmap(sample_url)  
  }
  
  return(closedpos(psb))
}


#* provide summary of long positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /long_positions_json
#* @serializer json
function(sample_url) {
  if(sample_url=="fakeit") {
      psb <- readRDS("fakeit.rds")  
  } else {
      psb<-"brak danych"
      psb <- pSBmap(sample_url)  
  }
  return(openlongpos(psb))
}

#* provide summary of short positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /short_positions_json
#* @serializer json
function(sample_url) {
  if(sample_url=="fakeit") {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb<-"brak danych"
    psb <- pSBmap(sample_url)  
  }
  return(openshortpos(psb))
}




#* @assets ./static /
list()
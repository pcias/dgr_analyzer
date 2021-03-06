# Copyright (C) 2021 pcs Przemyslaw Cias
# 
# This file is part of DGR Analyzer.
# 
# DGR Analyzer is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# DGR Analyzer-React is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with DGR Analyzer-React.  If not, see <http://www.gnu.org/licenses/>.


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
function(sample_url="test") {
  sample_url <- if_else(is.na(sample_url),"",sample_url)
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb <- pSBmap(sample_url)  
  }
  output <- pSBreactable(psb)
  
  return(output$closed)
}


#* provide summary of long positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /long_positions
#* @serializer htmlwidget
function(sample_url="test") {
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb <- pSBmap(sample_url)  
  }
  output <- pSBreactable(psb)
  
  
  return(output$long)
}

#* provide summary of short positions
#* @param sample_url sample_url from open session (portfel->pobierz->xls->copy link address)
#* @get /short_positions
#* @serializer htmlwidget
function(sample_url="test") {
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb <- pSBmap(sample_url)  
  }
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
function(sample_url="test") {
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
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
function(sample_url="test") {
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
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
function(sample_url="test") {
  if(sample_url %in% c('fakeit','Fakeit','test','Test')) {
    psb <- readRDS("fakeit.rds")  
  } else {
    psb<-"brak danych"
    psb <- pSBmap(sample_url)  
  }
  return(openshortpos(psb))
}

#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

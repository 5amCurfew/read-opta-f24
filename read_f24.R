library(dplyr)
library(tibble)
library(purrr)
library(xml2)

read_f24 <- function(xml_filename){
  
  print(read_xml(xml_filename, encoding = "", as_html = TRUE) %>% xml_find_all('//game'))
  
  ## convert to one row (default to max)
  to_row <- function(Q_values_matrix){
    
    max_values <- list()
    for (c in 1:NCOL(Q_values_matrix)) {
      
      test <- Q_values_matrix[,c]
      max_val <- test[ !is.na(test) ][ 1 ]
      max_values <- append( unlist(max_values), max_val )
      
    }
    results_Q <- t( as.data.frame( max_values ) )
    colnames(results_Q) <- colnames( Q_values_matrix )
    
    return(results_Q)
  }
  

  convert <- function(xml){
    
    ## event node header
    results <- as.data.frame( t( as.data.frame(xml$attrs) ),  stringsAsFactors = FALSE)
    rownames(results) <- NULL
    
    Qs <- lengths(xml$value)
    
    if(Qs > 0){
      ## create a list of qualifiers 
      q_base <- data.frame(stringsAsFactors = F)
      
      ## loop through qualifiers and bind
      for (Q in 1:Qs) {
        
        q_spread <- unlist( xml$value[1][[1]][Q] )
        Value <- ifelse( is.na(q_spread["value"]), 1, q_spread["value"] )
        holder <- data.frame(Q = as.character(Value), stringsAsFactors = F)
        colnames(holder) <- q_spread["qualifier_id"]  
        q_base <- bind_rows(q_base, holder)
        
      }
      
      Q_df <- to_row(q_base)
      rownames(Q_df) <- NULL  
      results <- cbind(results, Q_df)
    }
    
    return(results %>% arrange(min, sec, team_id, event_id))
  }
  
  
  #// Read in the XML File ----------------------------------------------------------------//
  
  data <- read_xml(xml_filename, encoding = "", as_html = TRUE)
  print(data %>% xml_find_all('//event'))
  
  #// Spilt the XML File ------------------------------------------------------------------//
  
  all_event_Q <- data %>% 
                  xml_find_all('//event') %>% 
                  map_df(~list(attrs = list(xml_attrs(.x)), 
                               value = list(map(xml_children(.x), xml_attrs))))
  
  #// Convert all events and store in a dataframe ----------------------------------------//
  
  events <- all_event_Q %>% 
              split(1:nrow(.)) %>% 
              purrr::map( convert ) %>% 
              dplyr::bind_rows()
  
  
  #// convert strings to numerics ---------------------------------------------------------//
  
  cols_to_numeric <- c('event_id', 'type_id', 'period_id', 'min', 'sec', 'team_id', 'outcome', 'x', 'y', 'player_id',  '140', '141', '212', '213', 'outcome')
  for(i in cols_to_numeric){
    events[,i] <- as.numeric( unlist(events[,i]) )
  }
  
  return(events)
  
}


read_events_min <- function(game){
  
  e <- read_xml(game) %>% 
    xml_find_all("//Event") %>%
    map(xml_attrs) %>%
    map_df( ~as.list(.) )
  
  cols_to_numeric <- c('event_id', 'type_id', 'period_id', 'min', 'sec', 'team_id', 'outcome', 'x', 'y', 'player_id')
  for(i in cols_to_numeric){
    e[,i] <- as.numeric( unlist(e[,i]) )
  }
  
  return(e %>% arrange(period_id, min, sec, team_id, event_id))
  
}


library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
as.tbl(read_events_min('f24-8-2019-1059926-eventdetails.xml'))

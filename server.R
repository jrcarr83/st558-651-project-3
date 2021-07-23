library(shiny)
library(tidyverse)

source('get_data.r')
source('nba_functions.r')

shinyServer(function(session, input, output) {
  #observe the data style radio buttons
  #if team, we don't want per30 stat type
  #if player, we do want to add per30 stat type
  observeEvent(input$data_style, {
    if (input$data_style == 'team') {
      updateSelectInput(session, 'stat_type',
                        choices = c("Season Totals" = "ttl",
                                    "Game Averages" = "avg"))
    } else {
      updateSelectInput(session, 'stat_type',
                        choices = c("Season Totals" = "ttl",
                                    "Game Averages" = "avg",
                                    "Per30" = 'per30'))
    }
  })
  
  dataSet<-reactive({
    if (input$data_style == 'team') {
      get_team_data(data, input$stat_type)
    } else if (input$data_style == 'player') {
      get_player_data(data, input$stat_type)
    } 
  })
  output$nba_data_table <- DT::renderDataTable(
                              DT::datatable(dataSet(), style='bootstrap',
                                            filter = 'top',
                                            options = list(scrollX = TRUE,
                                                           pageLength = 5)))
  
  output$download <- downloadHandler(
    filename = function(){
      paste0('NBA Data - ', gsub("-", ".", Sys.Date()),".csv")
    }, 
    content = function(fname){
      write.csv(dataSet(), fname)
    }
  )



})

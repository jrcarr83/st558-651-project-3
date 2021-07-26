library(shiny)
library(tidyverse)
library(DT)
library(hrbrthemes)
library(caret)

source('helpers/get_data.r')
source('helpers/nba_functions.r')

shinyServer(function(session, input, output) {
  #### Data page
  #### Data page
  #### Data page
  #### Data page
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
  
  ##this is for the dataset relative to the data page
  dataSet<-reactive({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Fetching Data...", value = 0)
    
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
  #output for dataset on data page
  output$download <- downloadHandler(
    filename = function(){
      paste0('NBA Data - ', gsub("-", ".", Sys.Date()),".csv")
    }, 
    content = function(fname){
      write.csv(dataSet(), fname)
    }
  ) #output button for dataset on datapage

  #### Exploration Page
  #### Exploration Page
  #### Exploration Page
  #### Exploration Page
  #create plot
  joined <- get_joined_data(data)
  #output plot
  output$expl_plot <- renderPlotly({
    #create plot
    if (input$graph_type == 'density') {
      g <- get_density_plot(joined, input$graph_var, 
                            input$home_away, input$b2b, input$game_result)
      
    } else if (input$graph_type == 'scatter') {
      g <- get_scatter_plot(joined, input$xaxis, input$yaxis,
                        input$home_away, input$b2b, input$game_result)
      
    } else if (input$graph_type == 'box') {
      g <- get_box_plot(joined, input$graph_var, 
                            input$home_away, input$b2b, input$game_result)
      
      
    }
    g
  })
  #get summary tables
  summary_stats <- reactive({
    graph_type <- input$graph_type
    sum_stat <- NULL
    xaxis <- NULL
    yaxis <- NULL
    if (input$graph_type == 'density' || input$graph_type == 'box') {
      sum_stat <- get_summary_stats(joined, input$graph_var, 
                                     input$home_away, input$b2b,
                                     input$game_result)
    } else {
      xaxis <- get_summary_stats(joined, input$xaxis, 
                                     input$home_away, input$b2b, 
                                     input$game_result)
      yaxis <- get_summary_stats(joined, input$yaxis, 
                                     input$home_away, input$b2b,
                                     input$game_result)
      sum_stat <- tibble(rbind(xaxis, yaxis))
    }
  })
  #output the summary table
  output$summary_table <- renderTable({ 
    summary_stats()
  })
  
  reac <- reactiveValues()
  observe({ 
    reac$var1 =  input$graph_var
  }) 
  observe({ 
    reac$var2 =  input$xaxis
  }) 
  observe({ 
    reac$var3 =  input$yaxis
  }) 
  observe({ 
    reac$graph_type =  input$graph_type
  }) 
  
  #plot text
  graph_text <- ''
  sum_text <- ''
  
  output$plot_info <- renderText({
    if (reac$graph_type == 'density') {
      graph_text <- paste0('Density plot for ', reac$var1)
    } else if (reac$graph_type == 'scatter') {
      sum_text <- paste0('Summary information for ', reac$var2, 
                         ' and ', reac$var3)
    } else if (reac$graph_type == 'box') {
      sum_text <- paste0('Summary information for ', reac$var1)
    }
  })
  output$summary_info <- renderText({sum_text 
    if (reac$graph_type == 'density') {
      sum_text <- paste0('Summary information for ', reac$var1)
    } else if (reac$graph_type == 'scatter') {
      sum_text <- paste0('Summary information for ', reac$var2, 
                         ' and ', reac$var3)
    } else if (reac$graph_type == 'box') {
      sum_text <- paste0('Summary information for ', reac$var1)
    }
    sum_text
  })
})



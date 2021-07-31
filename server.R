
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
  
  #### Model Page
  #### Model Page
  #### Model Page
  #### Model Page
  
  #listen for the action button to split data in test/training
  observeEvent(input$train_button, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Splitting data...", value = 0)
    
    temp <- input$train
    temp2 <- 100-input$train
    
    train_data <- get_training_data(model_data, input$train / 100)
    train <<- train_data$train
    test <<- train_data$test
    rm(train_data)
  
    popup_text <- paste0('Data split: ', temp, '% in training set and ',
                   temp2, '% in test set.')
    shinyalert("Training data split.", popup_text)
    rm(temp)
    rm(temp2)
  })
  
  #listen for the action button to split data in test/training
  observeEvent(input$fit_button, {
    #if the user is not on the model selection tab, switch
    updateTabsetPanel(session, "model_tabset",
                      selected = "panel_fit")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    ###lasso in progress yo
    progress$set(message = "Fitting lasso regression model...", value = 0)
    lasso_fit <- get_lasso_fit(input$model_vars1, 
                               input$model_vars2, 
                               input$model_vars3,
                               input$folds, 
                               input$repeats,
                               var_list)
    mod_list[[1]] <<- lasso_fit$lasso.fit  
    print(mod_list[[1]])
    lasso_info <- get_model_info(lasso_fit$lasso.fit, 
                                 lasso_fit$var_ct,
                                'lasso')
    if (is.null(lasso_fit)) {
      shinyalert("Error with lasso model", 'User did not select any variables')
    }
    
    ###regression tree in progress yo
    progress$set(message = "Fitting regression tree...", value = 0)
    tree_fit <- get_tree_fit(input$model_vars1, 
                               input$model_vars2, 
                               input$model_vars3,
                               input$folds, 
                               input$repeats,
                               var_list)
    mod_list[[2]] <<- tree_fit$tree.fit
    tree_info <- get_model_info(tree_fit$tree.fit, 
                                 tree_fit$var_ct,
                                 'tree')
    ###rf in progress yo
    progress$set(message = "Fitting random forest model (be patient)...", value = 0)
    rf_fit <- get_rf_fit(input$model_vars1, 
                         input$model_vars2, 
                         input$model_vars3,
                         input$folds, 
                         input$repeats,
                         var_list)
    
    mod_list[[3]] <<- rf_fit$rf.fit
    rf_info <- get_model_info(rf_fit$rf.fit, 
                              rf_fit$var_ct,
                              'rf')
    
    #plots
    output$lasso_tuning <- renderPlot({lasso_fit$lasso.tune})
    output$lasso_resid <- renderPlot({lasso_fit$lasso.resid})
    output$tree_tuning <- renderPlot({tree_fit$tree.tune})
    output$rf_tuning <- renderPlot({rf_fit$rf.tune})
    
    output$lasso_table <- renderTable(lasso_info)
    output$tree_table <- renderTable(tree_info)
    output$rf_table <- renderTable(rf_info)
    

  }) #fitbutton
  
  observeEvent(input$pred_button, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    #check if models are null, if so, print error to user and quit
    if (is.na(mod_list[[1]])) {
      shinyalert("Error predicting", 'You have not trained any models yet!')
    } else {
      ###predicting
      progress$set(message = "Predicting...", value = 0)
      if (input$radio_model == 'lasso') {
        results <- get_pred_results(mod_list[[1]])
      } else if (input$radio_model == 'tree') {
        results <- get_pred_results(mod_list[[2]])
      } else if (input$radio_model == 'rf') {
        results <- get_pred_results(mod_list[[3]])
      } 
      
      #get confusion matrix
      cm_plot <- get_cm_plot(results)
      #get results table
      result_tbl <- get_outcome_tbl(results)
      
      col_names <- c('Date', 'Team', 'Opp', 'Guess',
                     'Prediction', 'Actual', 'Pts Diff')
      
      output$cm_plot <- renderPlot(cm_plot)
      output$pred_results <- DT::renderDataTable(
                              DT::datatable(result_tbl, style='bootstrap',
                                            colnames = col_names,
                                            filter = c("none"),
                                            options = list(scrollX = TRUE,
                                                           searching = FALSE,
                                                           lengthChange = FALSE,
                                                           autoWidth = TRUE
                                                           )))
    }
    
  }) #predict button
  
  output$nba_bg <- renderImage({
    # Return a list containing the filename
    list(src = 'img/nba.jpg',
         contentType = 'image/png',
         width='100%',
         margin='0px',
         padding='0px',
         alt = "This is alternate text - don't fail me if this shows up")
  }, deleteFile = FALSE)
  
})



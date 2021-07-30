library(shiny)
library(tidyverse)
library(shinythemes)
library(markdown)
library(plotly)
library(caret)
library(shinyalert)

source('helpers/get_data.r')


model_css <- "
#model_type ~ .selectize-control .selectize-input {
  min-height: 30px;
  width:120px;
  font-size: 10px;
}

#pred_type ~ .selectize-control .selectize-input {
  min-height: 30px;
  width:120px;
  font-size: 10px;
}

#radio_model {
  font-size: 10px;
}

#folds {
  height: 30px;
  width:50px;
  font-size: 15px;
}
#repeats {
  height: 30px;
  width:50px;
  font-size: 15px;
}

#pred_results ~ .input {
  height:10px;
  min_height:10px;
}
"

navbarPage('NBA Boxscore Data', theme = shinytheme("cyborg"),
  tabPanel('About',
     # Change the font size.
     tags$style(
       ".shiny-image-output shiny-bound-output {
         height:200px;
       }"
     ),
     tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} 
                .selectize-dropdown { font-size: 10px; line-height: 10px; }"),
     fluidRow(style='margin:0px',
       imageOutput('nba_bg', height='100%')
     ),
     fluidRow(style="margin-top:0px; height:600px", 
       column(width=6,
         h5('About the Data'),
         HTML('<p style="font-size:8pt;"<br>
              The data was sourced from an R library package 
              called <b>nbastatR</b>, which scrapes data from sources such as
              NBA Stats API, Basketball Insiders, 
              Basketball-Reference, HoopsHype, and RealGM. <br><br>
              
              <a href="https://www.rdocumentation.org/packages/nbastatR/versions/0.1.10131" target="_blank">
              Link to Documentation for nbastatR</a></p>')
       ),
       column(width=6,
         h5('About the App'),
         HTML('<p style="font-size:8pt;"<br>
            This app takes NBA box score data, summarizes it and attempts 
            to predict the outcome of a game based on previous performance in the 
            season.</p>
            <ul style="font-size:8pt;">
            <li>Data: view the data as it comes from the source. You can 
            transform by team or player and export the data.</li>
            <li>Data Exploration: View the data as it has been transformed
            for modeling via plots and summary tables.</li>
            <li>Modeling: Fit a lasso, decision tree, and random forest 
            model to predict whether a team will win a game.</li>
            </ul>
           '
         )
       )
     )
     
      
  ), #tabpanel About Page
  tabPanel('Data',
    fluidRow(
      
      column(width=1), #spacing
      column(width=2,
             radioButtons("data_style", "Select type to average by:",
                               c("Team" = "team",
                                 "Player" = "player"))),
      column(width=2,
             selectInput("stat_type", "Stat type:",
                          c("Season Totals" = "ttl",
                            "Game Averages" = "avg"))),
      column(width=3), #spacing
      column(width=3, downloadButton('download',"Download the data")),
      column(width=1) #spacing
    ), #filter row
    fluidRow(
      column(width=1), #spacing
      column(width=10, 
             DT::dataTableOutput("nba_data_table")
      ), #data table column
      column(width=1) #spacing
    ) #dataTable Row
           
  ), #tabpanel Data Page
  tabPanel('Data Exploration',
    sidebarLayout(
      sidebarPanel(
        selectInput("graph_type", "Graph style:",
                    c("Density Plot" = "density",
                      "Scatter Plot" = "scatter",
                      "Box Plot" = "box") #list of vars
                    ), #select input
        conditionalPanel(condition=
            "input.graph_type == 'density' || input.graph_type == 'box'",
            selectInput("graph_var", "Variable of Interest:",
                        c("Points Scored" = "pts",
                          "Field Goal PCT" = "pctFG",
                          "Total Rebounds" = "treb",
                          "Assists" = "ast",
                          "Steals" = "stl",
                          "Blocks" = "blk",
                          "Turnovers" = "tov",
                          "Personal Fouls" = "pf") #list of vars
                        ) #selectInput
        ), #conditional panel
        conditionalPanel(condition="input.graph_type == 'scatter'",
          fluidRow(
            column(width=6,
              selectInput("xaxis", "X-Axis Variable:",
                           c("Points Scored" = "pts",
                             "Field Goal PCT" = "pctFG",
                             "Total Rebounds" = "treb",
                             "Assists" = "ast",
                             "Steals" = "stl",
                             "Blocks" = "blk",
                             "Turnovers" = "tov",
                             "Personal Fouls" = "pf"), #list of vars
                          selected='pts'
              ) #select input #1
            ), #column first input
            column(width=6,
               selectInput("yaxis", "Y-Axis Variable:",
                           c("Points Scored" = "pts",
                             "Field Goal PCT" = "pctFG",
                             "Total Rebounds" = "treb",
                             "Assists" = "ast",
                             "Steals" = "stl",
                             "Blocks" = "blk",
                             "Turnovers" = "tov",
                             "Personal Fouls" = "pf"), #list of vars
                           selected='pctFG'
            ) #column second input  
          )) #fluidRow #selectInput
        ), #conditional panel
        checkboxInput("home_away", "Fill on home/away?"),
        checkboxInput("b2b", "Panel by back-to-back flag?"),
        radioButtons("game_result", "Subset by Result",
                     c('Win or Lose' = 'win_or_lose',
                       'Wins only' = 'win_only',
                       'Losses only' = 'loss_only') #list of choices
        ) #radio buttons
        
      ), #sidebarPanel
      mainPanel(
        column(width=12,  style="color:white; font-weight:bold",
               textOutput("plot_info")),
        column(width=12, plotlyOutput(outputId = "expl_plot")),
        column(width=12, style="color:white; font-weight:bold",
               textOutput("summary_info")),
        column(width=12, style='overflow-x: scroll; font-size:10px',
               align="center", tableOutput('summary_table'))
      )
    )#sidebarLayout        
  ), #tabpanel Data Exploration Page
  tabPanel('Modeling', tags$style(model_css),
    sidebarLayout(
      sidebarPanel(
        fluidRow(useShinyalert(),
          column(width=8,
                 sliderInput("train", "Train/Test split:",
                             min = 20, max = 90, value = 80, step = 10)
          ),
          column(width=4,
            actionButton(style='background-color:#54C571; margin-top:40px',
                         inputId = "train_button", label="Split")
          )#button to lock in training/test data split
        ),
        fluidRow(
          column(width=1,
                 numericInput(inputId = "folds", "Folds", 
                              value = 5, min=2, max=5)
          ), #input for cv folds
          column(width=1),
          column(width=1,
                 numericInput("repeats", "Repeats", 
                              value = 5, min=1, max=5),
          ),
          column(width=1),
          column(width=5,
             selectInput("model_type",
                         "Model type",
                         c("Lasso Regression" = "lasso",
                           "Classificaton Tree" = "tree",
                           "Random Forest" = "rf")
             ) #model input
          ), #column drop down
          column(width=1)
        ), #fluidRow for sliders
        fluidRow(style='text-align:left;', 
                 'Select variables to include:'),
        fluidRow(style='height:125pt; overflow-y: scroll; overflow-x: hidden; font-size: 10px',
          column(width=4, 
            checkboxGroupInput("model_vars1", 
                             choices = var_list$names[1:8],
                             selected = var_list$names[1:8],
                             label=NULL
            )
          ), 
          column(width=4, 
                checkboxGroupInput("model_vars2", 
                                  choices = var_list$names[9:16],
                                  selected = var_list$names[9:16],
                                  label=NULL
                )
          ),
          column(width=4, 
                 checkboxGroupInput("model_vars3", 
                                    choices = var_list$names[17:24],
                                    selected = var_list$names[17:24],
                                    label=NULL
                 )
          ),
        ), #fluidRow variables
        fluidRow(style='margin-top:10pt;',
          column(width=4),
          column(width=2,
                 actionButton(style='background-color:#54C571;',
                              "fit_button", "Fit Model")),
          column(width=6)
        )#fluidRow fit model button
      ), #sidebar panel
      mainPanel(
        fluidRow(style="height:25pt;font-size: 10px",
          #store results from model fits in these data table
          column(width=4,
            tableOutput('lasso_table')
          ),#first dataframe for logistic results
          column(width=4,
            tableOutput('tree_table')
          ),#first dataframe for classification tree results
          column(width=4,
            tableOutput('rf_table')
          )#third dataframe for random forest results
        ), #fluid row for data tables
        tags$head(
          tags$style(type='text/css', 
                     ".nav-tabs {font-size: 10px} ")),    
        tabsetPanel(id='model_tabset', 
          tabPanel("Model Info",
          ), #tabPanel modelInfo 
          tabPanel("Model Fit", value='panel_fit',
            fluidRow(
              conditionalPanel(condition="input.model_type == 'lasso'",
                column(width=1),
                column(width=5,
                    plotOutput(outputId = "lasso_tuning")
                ), #column for tuning parameter 
                column(width=5,
                    plotOutput(outputId = "lasso_resid") 
                ), #column for fitting vs residuals
                column(width=1)
              ), #lasso condition
              conditionalPanel(condition="input.model_type == 'tree'",
                column(width=1),
                column(width=10,
                  plotOutput(outputId = "tree_tuning")
                ), #column for tuning parameter 
                column(width=1)
              ), #tree condition
              conditionalPanel(condition="input.model_type == 'rf'",
                column(width=1),
                column(width=10,
                  plotOutput(outputId = "rf_tuning")
                ), #column for tuning parameter 
               column(width=1)
              ), #tree condition
            ),
            fluidRow()
          ), #tabpanel model fit
          tabPanel("Prediction",
            fluidRow(style='margin-top:10px;',
              column(width=3,
                 selectInput("pred_type", label=NULL,
                             c("Table" = "tbl",
                               "Confusion Matrix" = "cm"))
              ),
              column(width=7,
                radioButtons(inputId = 'radio_model', 
                             label='Choose the model to make predictions', 
                             choices = c("Lasso Regression" = "lasso",
                                         "Classificaton Tree" = "tree",
                                         "Random Forest" = "rf"), 
                             selected = 'lasso',
                             inline = TRUE)
              ),
              column(width=2,
                     actionButton(style='background-color:#54C571',
                                  inputId = "pred_button", label="Predict!"))
            ),#fluidRow for first line (choose model and button)
            fluidRow(style='height:200pt; overflow-y: scroll; font-size: 10px',
              column(width=1),
              column(width=10,
                conditionalPanel(condition="input.pred_type == 'tbl'",
                  DT::dataTableOutput("pred_results")
                ),
                conditionalPanel(condition="input.pred_type == 'cm'",
                  plotOutput(outputId = "cm_plot")
                )
              ),
              column(width=1)
            ) #results in table form
          )
        )
        
      )#main panel
    )#sidebarlayout
    
  ) #tabpanel modeling Page
) #navbarpage
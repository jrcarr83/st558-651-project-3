library(shiny)
library(shinythemes)
library(markdown)
library(plotly)
library(caret)
library(shinyalert)

navbarPage('NBA Boxscore Data', theme = shinytheme("cyborg"),
  tabPanel('About',
     # Change the font size.
     tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} 
                .selectize-dropdown { font-size: 10px; line-height: 10px; }")
      
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
  tabPanel('Modeling',
    sidebarLayout(
      sidebarPanel(
        fluidRow(useShinyalert(),
          column(width=8,
                 sliderInput("train", "Train/Test split:",
                             min = 20, max = 80, value = 80, step = 10)
          ),
          column(width=4,
            actionButton(style='background-color:#54C571; margin-top:40px',
                         inputId = "train_button", label="Split")
          )#button to lock in training/test data split
        ),
        fluidRow(
          column(width=6,
                 numericInput("folds", "CV Folds:", 
                              value = 5, min=2, max=10)
          ), #slider for cv folds
          column(width=6,
                 numericInput("repeats", "CV Repeats:", 
                              value = 10, min=1, max=10)
          ) #input for cv repeats
        ), #fluidRow for sliders
        fluidRow(
          column(width=1),
          column(width=10,
             selectInput("model_type", "Select your model type:",
                c("Lasso Regression" = "lasso",
                  "Classificaton Tree" = "tree",
                  "Random Forest" = "rf"))
          ),
          column(width=1)
        ),#fluid row for model selection and 
        fluidRow(style='text-align:left', 'Select variables to include:'),
        fluidRow(style='height:72pt; overflow-y: scroll; font-size: 10px',
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
        fluidRow(
          column(width=4),
          column(width=2,
                 actionButton(style='background-color:#54C571;',
                              "fit_button", "Fit Model")),
          column(width=6)
        )#fluidRow fit model button
      ), #sidebar panel
      mainPanel(
        fluidRow(style="height:72pt;font-size: 10px",
          #store results from model fits in these data table
          column(width=4,
            tableOutput('lasso_table')
          ),#first dataframe for logistic results
          column(width=4,
            tableOutput('tree_table')
          ),#first dataframe for classification tree results
          column(width=4
          )#third dataframe for random forest results
        ), #fluid row for data tables
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
            ),
            fluidRow()
          ), #tabpanel model fit
          tabPanel("Prediction")
        )
        
      )#main panel
    )#sidebarlayout
    
  ) #tabpanel modeling Page
) #navbarpage
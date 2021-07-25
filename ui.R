library(shiny)
library(shinythemes)
library(markdown)
library(plotly)
library(caret)

navbarPage('NBA Boxscore Data', theme = shinytheme("cyborg"),
  tabPanel('About',
     # Change the font size.
     tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} 
                .selectize-dropdown { font-size: 10px; line-height: 10px; }"),
      
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
        column(width=12, plotOutput(outputId = "expl_plot")),
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
      ), #sidebar panel
      mainPanel(
        tabsetPanel(
          tabPanel("Model Info"), 
          tabPanel("Model Fit"), 
          tabPanel("Prediction")
        )
        
      )#main panel
    )#sidebarlayout
    
  ) #tabpanel Data Page
) #navbarpage
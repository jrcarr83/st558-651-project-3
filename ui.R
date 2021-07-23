library(shiny)
library(shinythemes)
library(markdown)

navbarPage('NBA Boxscore Data', theme = shinytheme("cyborg"),
  tabPanel('About'
    
    
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
  tabPanel('Data Exploration'
           
           
  ), #tabpanel Data Exploration Page
  tabPanel('Modeling'
           
           
  ) #tabpanel Data Page
) #navbarpage
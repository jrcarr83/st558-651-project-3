# ST558-651 Project 3

## Introduction
In this project we are finding a dataset that is interesting to us and creating a shiny app that allows the user to export the data, explore it, fit models, and create predictions from those models. I chose a dataset that was curated from an R package called `nbastatR`, which you can find <a href="https://www.rdocumentation.org/packages/nbastatR/versions/0.1.10131" target="_blank">here</a>. 

## Packages Requred
All packages are imported on a single R file (found in the helpers folder) called `load_packages.r`. The packages required are:

*  shiny: obviously
*  tidyverse: for computational stuff 
*  shinythemes: for dark mode
*  markdown: latex and markdown language
*  plotly: for plotly plots
*  shinyalert: creates a modal popup
*  DT: datatables found on the data tab and modeling tab
*  hrbrthemes: ggplot dark theme to match the dark shiny theme
*  caret: tuning models
*  likert: reverse-levels function in confusion matrix math

Here is a line of code to install all the packages you may need:
install.packages(c('shiny', 'tidyverse', 'shinythemes', 'markdown', 'plotly',
                   'shinyalert', 'DT', 'hrbrthemes', 'caret', 'likert'))

## Shiny App Description
There are four main tabs:
*  About: the front page with a brief description similar to what you see on this page
*  Data:  a data table you can export that contains what is essentially the base dataset scraped via `nbastatr`
*  Data Exploration: View either a density, scatter, or box plot choosing different variables, and split by home/away and a back-to-back flag. You can also filter games based on whether they were won or lost.
*  Modeling: The user can fit 3 models: a lasso regression, a regression tree, and a random forest model. The user can pick the training and test split ratio, the number of folds and times to repeat cross validation, and select the variables. It is defaulted to all variables since these are all feature selection and reduction models anyway. Once the models are fit, the user can pick one of the three to make predictions on the test set and see the results.

## Running the App
The user can paste the following code into R to run the app:
shiny::runGitHub(repo='st558-651-project-3', 
                 username='jrcarr83', 
                 ref='main')

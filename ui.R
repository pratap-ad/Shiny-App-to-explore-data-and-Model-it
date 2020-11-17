

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)

#Load data
mbl_pitching<- read.csv("Pitching.csv")


# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Project III ST558"),
        dashboardSidebar(
            sidebarMenu(
            menuItem("Information about the data", tabName = "information"),
            menuItem("Data exploration", tabName = "dataexp"),
            menuItem("Principal Component Analysis", tabName = "PCA"),
            menuItem("Modeling", tabName = "modelg"),
            menuItem("Dataset", tabName = "fulldata")
            )),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dataexp",
                    fluidRow(
                        column(width = 4,
                        box(selectInput("var", "Select the Team ID", 
                                        selected = "ARI",
                                        choices = levels(as.factor(mbl_pitching$teamID))),
                         
                         sliderInput("bins", "Number of bins",
                                min=1, max=50, value=10))),
                        
                        box("Histogram for wins, teamID= ", uiOutput("sumh") , status = "primary", solidHeader = T,
                            collapsible = T, plotOutput("hist")),
                        
                        box("Summary statistics for some of the attributes", verbatimTextOutput("summ")),
                        
                        box("Data Table for the team ID you select", dataTableOutput ("table")),
                        
                        column(width = 12,
                        box("Bar Plot", status = "primary", solidHeader = T,
                            collapsible = T, plotOutput("bar"))),
                       
                        #box plot by team ID for seleceted year
                        box(selectInput("year", "Select the Year", 
                                        selected = "2015",
                                        choices = levels(as.factor(mbl_pitching$yearID)),
                                       ),
                            br(),
                            h3("Highest number of wins by any team=", textOutput("hiwin"))
                            ),
                        box("bar plot for Wins by team id", plotOutput("teambar")),
                        
                        box("Scatter Plot", plotOutput("scat"))
            )),
            
            
            


            
#Principal COmponent Analysis

tabItem(tabName = "PCA",
        fluidRow(
            box(selectizeInput("pcompA", "Select PC for x- axis",
                               selected = "1",
                               choices = as.character( c(1: 18) ))),
            box(selectizeInput("pcompB", "Select PC for y- axis",
                               
                               selected = "2",
                               choices = as.character( c(1: 18) ))),
            
            
             plotOutput ("pc")
        )),
        



#Modeling Page

tabItem(tabName = "modelg",
        fluidRow(
            box(selectInput("mvar", "Select the Team ID", 
                            selected = "ARI",
                            choices = levels(as.factor(mbl_pitching$teamID))),
                selectInput("ntre", "Number of Trees to fit:", 
                            selected = "100",
                            choices = as.character(c(1:300)))
           
                
                
        ),
        plotOutput("mdlfit")
        
        )),






#Dataset page

tabItem(tabName = "fulldata",
        box(downloadButton("downloadData", "Download"), 
            br(),
            h4("Select options below to subset the data"),
            br(),
            
            varSelectInput("dtsetvar", "Select variables to subset", mbl_pitching, multiple = T),
            
            selectInput("dtyear", "Select the Year", 
                        choices = levels(as.factor(mbl_pitching$yearID)), multiple = T),
            selectInput("dtteam", "Select the Team ID", 
                        choices = levels(as.factor(mbl_pitching$teamID)), multiple = T),
                        
            ),
        
        #box(datatable(mbl_pitching)),
        box( dataTableOutput("dtset") )
),


            
# Information page            
            tabItem(tabName = "information",
            h2("Description"),
           br(),
            h4(" Here I am analysing the 'Baseball' data.  Amongst the many available datasets,
            'Pitching', (post-season pitching) data set is choosen to analyse it. The data 
            comprise the players from 1871 to 2016 AD, and their win, loss, win/loss rates etc.
               
               The data set is donloaded from this", 
               a(href= "https://www.kaggle.com/open-source-sports/baseball-databank", "link"),
               " -originally form kaggle." ),
           h3("The Data"),
           br(),
           h4("This data set has 44139 observation and 30 variables. Each player 
              is assigned a unique number (playerID). "), 
           br(), 
           h3("Attribute Information"),
           br(),
           h4("1 - playerID", br(), "2 - yearID", br(), "3 - stint", br(), "4 - teamID",
              br(), "5 - lgID", br(), "6 - W", br(), "7 - L", br(), "8 - G", br(), 
              "9 - GS", br(), "10 - CG", br(), "11 - SHO", br(), "12 - SV", br(), 
              "13 - IPouts", br(), "14 - H", br(), "15 - ER", br(), "16 - HR", br(), 
              "17 - BB", br(), "18 - SO", br(), "19 - BAOpp", br(), "20 - ERA", 
              br(), "21 - IBB", br(), "22 - WP", br(), "23 - HBP", br(),  
              "24 - BK", br(), "25 - BFP", br(), "26 - GG", br(), "27 - R", br(), 
              "28 - SH", br(), "29 - SF", br(), "30 - GIDP"),
             #h3(textOutput("attribute")) 
           
            )
                )
            )
        )
    )


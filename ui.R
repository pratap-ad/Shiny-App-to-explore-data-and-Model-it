

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(plotly)

#Load data
mbl_pitching<- read.csv("Pitching.csv")
predvar<-levels (as.factor( mbl_pitching %>% filter(yearID==2015) %>% select(teamID)))

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
        
  #Data Exploration page
        dashboardBody(
            tabItems(
                tabItem(tabName = "dataexp",
                        fluidRow( h2("2. Data Exploration"),br(), br(),
                            column(width = 6,
                                   box(selectInput("var", "Select the Team ID", 
                                                   selected = "ARI",
                                                   choices = levels(as.factor(mbl_pitching$teamID))),
                                       
                                       sliderInput("bins", "Number of bins",
                                                   min=1, max=50, value=10))),
                            
                            box("Histogram for wins, teamID= ", uiOutput("sumh") , status = "primary", solidHeader = T,
                                collapsible = T, plotOutput("hist")),
                            
                            box("Summary statistics for some of the attributes", verbatimTextOutput("summ")),
                            
                            box("An overview of Data Table for the team ID you select", dataTableOutput ("table")),
                            
                            
                            
                            #bar plot by team ID for seleceted year
                            h3("Bar plot fot the team selected above and the game year below:"),
                            box(selectInput("year", "Select the Year", 
                                            selected = "2015",
                                            choices = levels(as.factor(mbl_pitching$yearID)),
                            ),
                            br(),
                            h4("Cumulative number of wins recorded the team and year selected=", textOutput("cumwin")),
                            h4("Highest number of wins recorded on the year selected=", textOutput("hiwin"))
                            ),
                            
                            column(width = 12,
                                   box("Bar Plot", status = "primary", solidHeader = T,
                                       collapsible = T, plotOutput("bar"))),
                            
                            
                            box("Scatter Plot", plotlyOutput("scat"))
                        )),
                
                
                
                
                
                
  #Principal COmponent Analysis
                
                tabItem(tabName = "PCA",
                        fluidRow( h2("3. Principal Component Analysis"), br(), br(),
                            
                            h4("Principal Componets printed:"),
                            verbatimTextOutput("pcTable"), br(),
                            
                            plotOutput("pcvaris"),
                            
                            h4("Plot of Cumulative variability to the scale of 1"),      
                            plotOutput("pcCum"), br(), br(),
                            
                            h3("A look at the biplot of PCs"), br(),
                            
                            box(selectizeInput("pcompA", "Select PC for x- axis",
                                               selected = "1",
                                               choices = as.character( c(1: 16) ))),
                            box(selectizeInput("pcompB", "Select PC for y- axis",
                                               
                                               selected = "2",
                                               choices = as.character( c(1: 16) ))),
                            
                            
                           box( plotOutput ("pc") ), br(), br(),
                            
                            h3("Looking at the plot, the number of PCs explaing most of the variability be counted
                               towards the predictors. A thumb rule may be considered as where the bend is most. 
                               The variables can be selected by looking at very first printed table of PCs which 
                               has explained the most variability.")
                        )),
                
                
                
                
 #Modeling Page
                
                tabItem(tabName = "modelg",
                        fluidRow( h2("4. Modeling"), br(),
                                  h4(" Here we will use the data available upto year of 2014. We also
                                  have the options available to choose team ID and number of trees to
                                  fit. The model fitted will predict 'W' (Wins) for the year of 2015 AD. Then we will 
                                     compare the outcome of the prediction by the model fit  for both of the mothods
                                     i.e. Random Forest and Linear model we used to predict."),
                                  h4("The variables used for the prediction are : yearID, stint, 
                                  W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA and R. But will also have option 
                                                 to select the variables to use in the model from the list presented
                                     here" ), br(), br(),
                            box(column(width = 6,
                                selectInput("Teamtopred", "Select the Team ID", 
                                            selected = "ARI",
                                            choices = unique( mbl_pitching %>% filter(yearID==2015) %>% select(teamID))),
                                selectInput("ntre", "Number of Trees to fit:", 
                                            selected = "100",
                                            choices = as.character(c(1:300))),
                                selectInput("predv", "Select the predictors",
                                            selected = "BAL",
                                            choices = c(" yearID", "stint", "W", "L", "G", "GS", "CG", 
                                                 "SHO", "SV", "H", "ER", "HR", "BB", "SO", "ERA", "R"), multiple = T)
                                
                                
                                
                            )),
                            
                            h3("Outcome of fitted Random model Predictions:"),
                            box(verbatimTextOutput ("rfRMSE")),
                            
                            h3("Outcome of fitted Linear Model Predictions:"),
                            box(verbatimTextOutput ("lmRMSE")),
                            
                         
                            box(  h4("Table showing the prediction values for W(Wins) for both the linear model and  random forest fits. 
                               'W' is the actual wins and W_LM/W_RF are the predicted Wins for linear model and random forest 
                               model fits respectively."),
                              dataTableOutput("predtable")),
                            
                            h3("Select the method you would like to use"),
                            box(radioButtons("modeltofit", "Method:", 
                                             selected= "Linear Model",
                                               choices=c("Random Forest", "Linear Model"))),
                            
                            h3("As the number of wins in the fractions is not intuitive it is rounded to whole number"),
                            
                          
                            
                            box(downloadButton("downloadPred", "Download"),
                                dataTableOutput("predtable2")),
                            box(dataTableOutput("d1_rf"))
                        )),
                
                
                
                
                
                
                #Dataset page
                
                tabItem(tabName = "fulldata",
                       fluidRow( h2("5. Dataset"), br(), br(),
                                 h4("The intention of this page is to allow the user navigate throught the whole
                                    data set and play around to choose the attributes as per the requirement of
                                    user. Here you can also subset the data set and downoload as '.csv' file.
                                    While subsetting the data please remember that not all of the teams were in
                                    existance throught years of data available. So you may encounter a data table 
                                    reflecting 0 observations for some of the teamID and yearID selected"), br(),
                                 
                        box( 
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
                        box(downloadButton("downloadData", "Download"),
                            dataTableOutput("dtset") )
                        
                )),
                
                
                
                # Information page            
                tabItem(tabName = "information",
                        h2("This app will have 5 different componets"),
                        br(), br(),
                        
                        h3("1. Information About the Data"), 
                          h4("This page describes the data, purpose of the app and how to navigate it. "),
                        h3("2. Data Exploration"),
                          h4("In this page common numerical and graphical summaries can be generated for
                             a particular team as per user selection."), 
                        
                        h3("3. Principal Component Analysis"),
                          h4("In this page PCA and plots can be genenerated."),
                        
                        h3("4. Modeling"),
                          h4("A page for modeling."), 
                        
                        h3("5. Dataset"),
                          h4("In this page user can scroll through the data, subset it and save the data as a '.csv' file."),
                        
                        
                       br(),
                       br(),
                        
                        
                        
                        h3("1. Information About the Data"), br(), 
                        h2("Data Set Information"),
                        br(),
                        
                        h4(" This data set is donloaded from this", 
                           a(href= "https://www.kaggle.com/open-source-sports/baseball-databank", "link"),
                           " -form kaggle.", "This is a baseball 'Pitching' dataset. 
                           The data comprise the players and games informations from 1871 to 2015 AD. 
                           Each player is assigned a unique number (playerID). We can see 146 unique team IDs
                           for the period data recorded. Which were later chagend the name. For the latest yearID
                           2015, there are 30 unique teams participating in the league. So, for each year selected,
                           will not have the data for every team and viceversa. A brief atribute 
                           informations are also provided below. More information about the
                            Major League Baseball (MLB) can be found", a(href= "https://www.mlb.com/", "here.")),
                           
                        
                        
                        br(), 
                        h3("Attribute Information"),
                        br(),
                        h4("1 - playerID", "- Player ID code", br(), "2 - yearID", "- Year", br(), 
                           "3 - stint", "- player's stint (order of appearances within a season", br(), 
                           "4 - teamID", "- Team; a factor", br(), "5 - lgID","- League; a factor with 
                           levels AA AL FL NL PL UA", br(), "6 - W","- Wins", br(), "7 - L", "- Losses", 
                           br(), "8 - G", "- Games", br(), "9 - GS", "- Games Started", br(), "10 - CG", 
                           "- Complete Games", br(), "11 - SHO", "- Shutouts", br(), "12 - SV", "- Saves",
                           br(), "13 - IPouts", "- Outs Pitched (innings pitched x3", br(), "14 - H", "- Hits", 
                           br(), "15 - ER", "- Earned Runs", br(), "16 - HR","- Homeruns", br(), "17 - BB", 
                           "- Walks", br(), "18 - SO","- Strikeouts", br(), "19 - BAOpp", "-Opponent's 
                           Batting Average", br(), "20 - ERA", "- Earned Run Averages",  
                           br(), "21 - IBB", "- Intentional Walks", br(), "22 - WP","- Wild Pitches", br(), 
                           "23 - HBP", "- Batters Hit By Pitch", br(),  "24 - BK","-Balks", br(), "25 - BFP",
                           "- Batters faced by Pitcher", br(), "26 - GF","- Games Finished", br(), "27 - R", 
                           "- Runs Allowed", br(), "28 - SH","- Sacrifices by opposing batters", br(), 
                           "29 - SF","- Sacrifice files by opposing batters", br(), 
                           "30 - GIDP", "- Grounded into double plays by opposing batter"),
                        br(),
                        br(),
                        h3("Purpose of the App"),
                        br(), 
                        h4("Our goal here will be to fit the model and predict the 'W' (Wins) for one user picked team for the year of 
                        2015 and measure the statistics, how accurately can it be predicted. We will be using  the Random Forest model and
                           Lenear model to fit the model and predict the wins. Then choose the best one."),
                        
                       
                        
                        h5(" The Multiple Linear Regression Equation: ", br(), "Matrix Form:"), 
                      
            #Mathematical expression
                       withMathJax(),
                       helpText(' $${\\textbf{y}}= Xb + E$$ 

 and a fraction $$1-\\frac{1}{2}$$'),
                       
                       helpText('$$\\sum$$ x')
           
                                 
                        
                )
            )
        )
    )
)


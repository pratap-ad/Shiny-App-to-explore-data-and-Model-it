# Project-III-ST-558

## Creating a Shiny App
#### The goal of this project is to create a shiny app that can be used to explore data and model it.
#### I have used the `Pitching` data set of Major Baseball League (MBL).


### Packages needed to install and call the library
```{r}
install.packages("shiny", "shinydashboard", "tidyverse", "dplyr", "ggplot2", "stringr", "caret", "DT", "plotly", "tree", "rattle", "randomForest" )


library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(plotly)
library(tree)
library(rattle)
library(randomForest)
```


### Code to run the repo from **RStudio:**
```
shiny::runGitHub("Project-III-ST-558", "pratap-ad", ref = "main")


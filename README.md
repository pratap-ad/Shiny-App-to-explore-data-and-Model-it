# Project-III-ST-558

## Creating a Shiny App
#### The goal of this project is to create a shiny app that can be used to explore data and model it.
#### I have used the `Pitching` data set of Major Baseball League (MBL).


### Packages needed to install and call the library
```{r}
install.packages(c("shiny", "shinydashboard", "tidyverse", "dplyr", "ggplot2", "stringr", 
                    "caret", "DT", "plotly", "tree", "rattle", "randomForest" ))


packages<- c("shiny", "shinydashboard", "tidyverse", "dplyr", 
             "ggplot2", "stringr", "caret", "DT", "plotly", 
             "tree", "rattle", "randomForest" )

lapply(packages, library, character.only= TRUE)
```


### Code to run the repo from **RStudio:**
Please copy below code and run on R studio console to run the app.

shiny::runGitHub("Shiny-App-to-explore-data-and-Model-it", "pratap-ad", ref = "main")


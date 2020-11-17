
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(tree)
library(rattle)
library(randomForest)
library(plotly)

#install.packages("RWeka")
#library(RWeka)

#library(sos)


#read the data
#testdta<- read.arff("Amazon_initial_50_30_10000.arff")

#td<- read.csv("Amazon_initial_50_30_10000.arff", header=F, comment.char="@")
#tda<- read_csv ("Amazon_initial_50_30_10000.arff", header=F, comment.char="@")

mbl_pitching<- read.csv("Pitching.csv")
name<- names(mbl_pitching)

# Define server 
shinyServer(function(input, output, session) {


    pv<- reactive({
        pv1<- mbl_pitching %>% filter(teamID == input$var)
      
    })
    
#dynamicUI
    output$sumh<- renderUI({(
      h4(toupper(str_to_title(input$teamID)))
    )}) 
  
    

    
     
#Histogram plot        
output$hist<- renderPlot({
  ggplot(pv(), aes(x=pv()$W, ..density..)) +geom_histogram(bins=input$bins) + ylab("Density") + xlab("Wins") +
    geom_density(col="red", lwd=2, adjust=1)  
  
  #hist( pv()$W, breaks = input$bins)
})

#summary 
output$summ<- renderPrint({
    summary(mbl_pitching %>% filter(teamID==input$var)  %>%  select(W, L, ER, ERA, yearID))
})

output$table<- renderDataTable({
    datatable( pv() )
})


#Highest wins

output$hiwin<- renderText({
  dta4<- mbl_pitching %>% filter(yearID== as.numeric(input$year)) %>% group_by(teamID) %>% 
    summarise_at(vars(W), list(totwin=sum) )
  max(dta4$totwin)
})


#Bar plot of wins for team choosen
output$bar<- renderPlot({
    ggplot(data=pv()%>% filter(yearID== as.numeric(input$year)) , aes(x= W)) + geom_bar(aes(fill=playerID) ) + 
    scale_fill_discrete(name="Player ID") + xlab("Wins")
})

#team bar plot 

output$teambar<- renderPlot({
  dta3<- mbl_pitching %>% filter(yearID== as.numeric(input$year))
  ggplot(data= dta3, 
         aes(x= dta4$totwin ) ) + geom_bar(aes(fill= teamID, position = "dodge")) + 
    xlab("Wins") 


})


#capture all variables of the dataset
output$attribute<- renderText({
print(t(names(mbl_pitching)))
})




output$attribute<- renderText({
  print(names(mbl_pitching))
})


#scatter Plot

output$scat<- renderPlot({
scp<-  ggplot(pv(), aes(x=W, y=G)) + geom_point(aes(color=teamID)) +
    geom_smooth(method = 'lm', color='green') + ylab("Goals") + xlab("Wins")

ggplotly(scp)
})


#Principal Component Analysis

output$pc<- renderPlot({
  
  pc<-  prcomp(dt<- na.omit(select(mbl_pitching, yearID, stint, W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA, R) ), 
               center = T, scale= T)
par(mfrow=c(1,3))
plot(cumsum (pc$sdev^2/sum(pc$sdev^2)), ylim=c(0,1), type='b', 
     ylab = "Cummulative variances in reduced scale to 1")
  biplot(pc, xlabs=rep(".", nrow(dt)), cex=2, 
         choices = c(as.numeric(input$pcompA) , as.numeric(input$pcompB)),
         xlab = "Plot for Principal Components")

})



#Modeling


output$mdlfit<- renderPrint({
  
  pv2<-  na.omit( mbl_pitching %>% select( teamID, playerID, yearID, stint, W, L, G, GS, CG, 
                        SHO, SV, H, ER, HR, BB, SO, ERA, R) %>% 
    filter(teamID == "BAL") )
  
  #set seed
  set.seed(3)
  #train and test dataset 
  train<- sample(1:nrow(pv2), size = nrow(pv2)*0.8)
  test<- dplyr::setdiff(1:nrow(pv2), train)
  
  #Fit the regression tree model
  treeFit<- tree(W~. -teamID -playerID , data=pv2 )
 # summary(treeFit)
 # text(treeFit)
  #plot(treeFit)
  
  #cvTree<- cv.tree(treeFit)
  
 #plot(cvTree$size, cvTree$dev, type = "b")
  
  trainData<- pv2[train,-c(1,2) ]
  testData<- pv2[test, -c(1,2) ]
  mod1Fit<- train(W~., data=trainData,
                  method="rpart",
                  preProcess= c("center", "scale"),
                  trControl= trainControl(method = "cv"))
  
  
  
  fancyRpartPlot(mod1Fit$finalModel)
  
  mod1Fit$bestTune
  
  #prediction
  pred<- predict(mod1Fit, newdata = testData)
  postResample(pred, testData$W)
  
  #linear model fit
  lmFit<- train(W~., data=trainData,
               method= "lm")
  predlm<- predict(lmFit, newdata= testData)
  postResample(predlm, testData$W)
  
  
  
  
  #Random Forest method
  rfFit<- randomForest(W~., data=trainData,
                      mtry=ncol(trainData)/3,
                      ntree= as.numeric(input$ntre), importance= T)
  rfPred<- predict(rfFit, newdata= dplyr::select(testData, -W))
  postResample(rfPred, testData$W)
  
  
  
  #Tuning Parameters
  tune1<- 1:5
  tune2<- letters[1:4]
  expand.grid(name1= tune1, name2= tune2)
  
 
#Fit model using data before 2015 and predict wins for 2015
  #train and test data sets
  train2015<- pv2 %>% filter(yearID !=2015)
  test2015<- pv2 %>% filter(yearID ==2015)
  
  #random forest method
  rfFit2015<- randomForest(W~., data=train2015,
                       mtry=ncol(train2015)/3,
                       ntree=200, importance= T)
  rfPred2015<- predict(rfFit2015, newdata= dplyr::select(test2015, -W))
  postResample(rfPred2015, test2015$W)
  
  
  
  
})








# Full data table with options to subset attributes





# Reactive value for data Table
datasetInput<- reactive({
  if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 &  length(input$dtteam)==0) return( datatable(dtFull<- mbl_pitching))
  if(length(input$dtsetvar) != 0 & length(input$dtyear)==0 &  length(input$dtteam)==0) return( datatable(dtFull<- mbl_pitching %>% dplyr::select(!!!input$dtsetvar)))
  if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 &  length(input$dtteam)==0) return( datatable(dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear))))
  if(length(input$dtsetvar) != 0 & length(input$dtyear)!=0 &  length(input$dtteam)==0) return( datatable(dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear)) %>% dplyr::select(!!!input$dtsetvar) ))
  
  if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 &  length(input$dtteam) !=0) return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam))))
  if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 &  length(input$dtteam)!=0) return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear))))
  if(length(input$dtsetvar) != 0 & length(input$dtyear) ==0 &  length(input$dtteam) !=0) return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam)) %>% select(!!!input$dtsetvar)) )
  
  datatable(dtFull<- mbl_pitching  %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear)) %>% dplyr::select(!!!input$dtsetvar) )
})




#datasetInput <- reactive({
#  switch(input$dataset,
#         "Pitching" = dtFull() )
#})

#Table of selected dataset

output$dtset<- renderDataTable({
  datasetInput()
})



# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("Pitching", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(as.data.frame( datasetInput() ), file, row.names = FALSE)
  }
)



})






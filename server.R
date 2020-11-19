# Project III, ST 558
#Author: Pratap Adhikari

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
    output$hist<- renderPlotly({
        hist1<- ggplot(pv(), aes(x=pv()$W, ..density..)) +geom_histogram(bins=input$bins) + ylab("Density") + xlab("Wins") +
            geom_density(col="red", lwd=2, adjust=1)  
        
        ggplotly(hist1)
    })
    
    #summary 
    output$summ<- renderPrint({
        summary(mbl_pitching %>% filter(teamID==input$var)  %>%  select(W, L, ER, ERA))
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
#Cumulative wins by the selected team
    output$cumwin<- renderText({
      dta5<- mbl_pitching %>% filter(yearID== as.numeric(input$year) & teamID== input$var)  %>% 
        summarise_at(vars(W), list(cum_win=sum) )
      max(dta5$cum_win)
    })
    
    
    #Bar plot of wins for team choosen
    output$bar<- renderPlotly({
        bar1<- ggplot(data=pv()%>% filter(yearID== as.numeric(input$year)) , aes(x= W)) + geom_bar(aes(fill=playerID) ) + 
            scale_fill_discrete(name="Player ID") + xlab("Wins")
        ggplotly(bar1)
    })
    
    #team bar plot 
    
    #output$teambar<- renderPlot({
     #   dta3<- mbl_pitching %>% filter(yearID== as.numeric(input$year))
      #  ggplot(data= dta3, 
       #        aes(x= dta3$totwin ) ) + geom_bar(aes(fill= teamID, position = "dodge")) + 
        #    xlab("Wins") 
        
        
    #})
    
    
    #capture all variables of the dataset
    output$attribute<- renderText({
        print(t(names(mbl_pitching)))
    })
    
    
    
    
    output$attribute<- renderText({
        print(names(mbl_pitching))
    })
    
    
    #scatter Plot
    
    output$scat<- renderPlotly ({
        scp<-  ggplot(pv(), aes(x=W, y=G)) + geom_point(aes(color=teamID)) +
            geom_smooth(method = 'lm', color='green') + ylab("Games Played") + xlab("Wins")
        
        ggplotly(scp)
    })
    
    
#Principal Component Analysis
  #PC table
    output$pcTable<- renderPrint({
      
      pc<-  prcomp(dt<- na.omit(select(mbl_pitching, yearID, stint, W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA, R) ), 
                   center = T, scale= T)
      pc
      
    })
 
    #PC  variances plot
    output$pcvaris<- renderPlot({
      
      pc<-  prcomp(dt<- na.omit(select(mbl_pitching, yearID, stint, W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA, R) ), 
                   center = T, scale= T)
      
      plot((pc$sdev^2/sum(pc$sdev^2)), ylim=c(0,1), type='b', 
           ylab = "Variances", xlab = "PCs",
           main = "Plot of varibilty")
      
      
    })
       
  #PC cumulative variances plot
    output$pcCum<- renderPlot({
        
        pc<-  prcomp(dt<- na.omit(select(mbl_pitching, yearID, stint, W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA, R) ), 
                     center = T, scale= T)
        
        plot(cumsum (pc$sdev^2/sum(pc$sdev^2)), ylim=c(0,1), type='b', 
             ylab = "Cummulative variances in reduced scale to 1", xlab = "PCs",
             main = "Plot of cumulative varibilty")
    
        
    })
    
  #PCs biplot
    output$pc<- renderPlot({
      
      pc<-  prcomp(dt<- na.omit(select(mbl_pitching, yearID, stint, W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, SO, ERA, R) ), 
                   center = T, scale= T)
      
      plot(cumsum (pc$sdev^2/sum(pc$sdev^2)), ylim=c(0,1), type='b', 
           ylab = "Cummulative variances", main = "Cummulative variances in reduced scale to 1")
      biplot(pc, xlabs=rep(".", nrow(dt)), cex=2, 
             choices = c(as.numeric(input$pcompA) , as.numeric(input$pcompB)),
             main = "Plot for Principal Components")
      
    })
   
    
    
     
#Modeling
   
    modeldta<- reactive({
      
      pv2<-  na.omit( mbl_pitching %>% select( teamID, playerID, yearID, stint, 
                                        W, L, G, GS, CG, SHO, SV, H, ER, HR, BB, 
                                        SO, ERA, R)  %>% 
                        filter(teamID == input$Teamtopred) )
    })
    

    
    
    
    output$rfRMSE<- renderPrint({

      
      #Fit model using data before 2015 and predict wins for 2015
      #train and test data sets
      train2015<- modeldta() %>% filter(yearID !=2015) %>% select(-teamID, -playerID)
      test2015<- modeldta() %>% filter(yearID ==2015) %>% select(-teamID, -playerID)
      
      #random forest method
      rfFit2015<- randomForest(W~., data=train2015,
                               mtry=ncol(train2015)/3,
                               ntree=as.numeric(input$ntre), importance= T)
      rfPred2015<- predict(rfFit2015, newdata= dplyr::select(test2015, -W))
      c_rf<- as.data.frame(rfPred2015)
      postResample(rfPred2015, test2015$W)
      
      
 
      postResample(rfPred2015, test2015$W)
      
      
    })
    
    output$lmRMSE<- renderPrint({
      
      #train and test data sets
      train2015<- modeldta() %>% filter(yearID !=2015) %>% select(-teamID, -playerID)
      test2015<- modeldta() %>% filter(yearID ==2015) %>% select(-teamID, -playerID)
      # linear model fit
      lmFit2015<- train(W~., data=train2015,
                        method= "lm")
      
      predlm2015<- predict(lmFit2015, newdata= test2015)
      c_lm<- as.data.frame(predlm2015)
      postResample(predlm2015, test2015$W)
      
      
      postResample(predlm2015, test2015$W)
    })
    
    
  #Predicted table
     predTbl<- reactive({
   
     
       players<- unique(modeldta() %>% filter(yearID==2015 & teamID== input$Teamtopred) %>% select(playerID))
       #Fit model using data before 2015 and predict wins for 2015
       #train and test data sets
       train2015<- modeldta() %>% filter(yearID !=2015) %>% select(-teamID, -playerID)
       test2015<- modeldta() %>% filter(yearID ==2015) %>% select(-teamID, -playerID)
       
       #random forest method
       rfFit2015<- randomForest(W~., data=train2015,
                                mtry=ncol(train2015)/3,
                                ntree=as.numeric(input$ntre), importance= T)
       rfPred2015<- predict(rfFit2015, newdata= dplyr::select(test2015, -W))
       
       c<- as.data.frame(rfPred2015)
       
       lmFit2015<- train(W~., data=train2015,
                         method= "lm")
       
       predlm2015<- predict(lmFit2015, newdata= test2015)
       c_lm<- as.data.frame(predlm2015)
       
       winPridict<- data.frame(players, teamID= input$Teamtopred, test2015 %>% 
                                 filter(yearID==2015) %>% select(yearID, W), W_RF= round( rfPred2015, 0), 
                             W_LM=round(predlm2015,0)) 

    })
    
    
     output$predtable<- renderDataTable({
       
       #if (input$modeltofit == "Linear Method")
        # return(datatable(predTbl() %>% select(- W_RF) ))
       datatable(predTbl())
       
     })
     
     predTbl2<- reactive({
       if (input$modeltofit == "Random Forest")
        return( datatable( dt6<- predTbl() %>% select(- W_LM) ))
       
       datatable(dt6<- predTbl() %>% select(- W_RF) )
       
     })
     
     output$predtable2<- renderDataTable({
       dt7<- predTbl2()
     
       dt7
     })
     
     
     
# Downloadable csv of selected dataset ----
     predTbl3<- reactive({
       if (input$modeltofit == "Random Forest")
         return(  dt8<- predTbl() %>% select(- W_LM) )
       
       dt8<- predTbl() %>% select(- W_RF) 
       
     })
     
          output$downloadPred <- downloadHandler(
       filename = function() {
         paste("WinsPredict", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(as.data.frame( predTbl3() ), file, row.names = FALSE)
       }
     )
     
     
     

# Full data table with options to subset attributes
    
    
    # Reactive value for data Table
    datasetInput<- reactive({
        if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 &  length(input$dtteam)==0) 
          return( datatable(dtFull<- mbl_pitching))
        if(length(input$dtsetvar) != 0 & length(input$dtyear)==0 &  length(input$dtteam)==0) 
          return( datatable(dtFull<- mbl_pitching %>% dplyr::select(!!!input$dtsetvar)))
        if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 &  length(input$dtteam)==0) 
          return( datatable(dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear))))
        if(length(input$dtsetvar) != 0 & length(input$dtyear)!=0 &  length(input$dtteam)==0) 
          return( datatable(dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear)) %>% dplyr::select(!!!input$dtsetvar) ))
        
        if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 &  length(input$dtteam) !=0) 
          return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam))))
        if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 &  length(input$dtteam)!=0) 
          return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear))))
        if(length(input$dtsetvar) != 0 & length(input$dtyear) ==0 &  length(input$dtteam) !=0) 
          return( datatable(dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam)) %>% select(!!!input$dtsetvar)) )
        
        datatable(dtFull<- mbl_pitching  %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear)) 
                  %>% dplyr::select(!!!input$dtsetvar) )
    })
    
    #Table of selected dataset
    
    output$dtset<- renderDataTable({
      datasetInput()
    })
    
    
    
#reactive data table for download options with out datatable() 
    datasetInput2<- reactive({
      
      if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 & length(input$dtteam)==0) return( dtFull<- mbl_pitching)
      
      if(length(input$dtsetvar) != 0 & length(input$dtyear)==0 & length(input$dtteam)==0) return( dtFull<- mbl_pitching %>% dplyr::select(!!!input$dtsetvar))
      
      if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 & length(input$dtteam)==0) return( dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear)))
      
      if(length(input$dtsetvar) != 0 & length(input$dtyear)!=0 & length(input$dtteam)==0) return( dtFull<- mbl_pitching %>% filter(yearID== c(input$dtyear)) %>% dplyr::select(!!!input$dtsetvar) )
      
      
      
      if(length(input$dtsetvar) == 0 & length(input$dtyear)==0 & length(input$dtteam) !=0) return( dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam)))
      
      if(length(input$dtsetvar) == 0 & length(input$dtyear)!=0 & length(input$dtteam)!=0) return( dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear)))
      
      if(length(input$dtsetvar) != 0 & length(input$dtyear) ==0 & length(input$dtteam) !=0) return( dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam)) %>% select(!!!input$dtsetvar))
      
      
      
      dtFull<- mbl_pitching %>% filter(teamID== c(input$dtteam), yearID== c(input$dtyear)) %>% dplyr::select(!!!input$dtsetvar) 
      
      
      
    })
  
    
    
    
    # Downloadable csv of selected dataset 
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Pitching", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame( datasetInput2() ), file, row.names = FALSE)
        }
    )
    
    
    
})






library(shiny)
library(shinydashboard)
library(caret)
library(mda)
library(earth)
library(tidyr)
library(rCharts)
cranid <<- readRDS("./data/cranid.rds")
namekeeper <- levels(cranid$Ancestry2)
fdaFit1 <<- readRDS("./data/model.rds")

shinyServer(function(input, output) {
   slideValues <- reactive({
      target <<- rbind(c(input$GOL, input$NOL, input$BNL, input$BBH, input$XCB,
                         input$XFB, input$AUB, input$ASB, input$BPL, input$NPH,
                         input$NLH, input$OBH, input$OBB, input$JUB, input$NLB,
                         input$MAB, input$ZMB, input$SSS, input$FMB, input$NAS,
                         input$EKB, input$DKB, input$WMH, input$FRC, input$FRS,
                         input$PAC, input$PAS, input$OCC, input$OCS))
      target <<- matrix(target, c(1,29), byrow = T)
      target <<- as.data.frame(target)
      names(target) <<- names(cranid)[1:29]
      ancesCalc <<- predict(fdaFit1, target, type = "prob")
      names(ancesCalc) <<- namekeeper
      par(mar = c(9,3,1,1))
      #       ancesCalc <<- gather(ancesCalc)
      #       names(ancesCalc) <<- c("Groups", "Probability")
      barplot(as.matrix(ancesCalc), main = "Population groups",
              ylim = c(0,1), las=2, cex.names = 0.8)
      # rPlot(Probability ~ Groups, data = ancesCalc, type = "bar")
      
   })
   output$plot <- renderPlot({
      slideValues()
   })
   
})

####


# ancesCalc %>% ggvis(~Probability, ~Groups) %>% layer_bars() %>%
#    add_axis("x", title = "", properties = axis_props(
#       labels = list(angle = 12, align = "left", fontSize = 9)
#    )) %>% bind_shiny("ggvis")
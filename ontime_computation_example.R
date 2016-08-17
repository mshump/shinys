library(shiny)
library(markovchain) 

runApp(list(
  ui=pageWithSidebar(headerPanel("Recommendation System - POC"),
                     sidebarPanel( #textInput("text1", "Column 1"),
                                  h4("Your Recommendations")
                                  ,actionButton("choice1", type="button", "Action 1", class="btn-default  btn-lg")
                                  ,actionButton("choice2", type="button", "Action 2", class="btn-info  btn-lg")
                                  ,actionButton("choice3", type="button", "Action 3", class="btn-success  btn-lg")
                                  ,conditionalPanel(condition = "output.p_c1 < 0.1" ,
                                    actionButton("choice4", type="button", "SAVE", class="btn-warning  btn-lg")
                                  )  
                                  ,conditionalPanel(condition = "output.p_c1 > 0.4" ,
                                    actionButton("choice5", type="button", "SELL", class="btn-danger  btn-lg"))
                                  ),
                     mainPanel(#tableOutput("table1")
                       
                       h5("Intervention Thresholds: SAVE => <0.2, SELL => >0.4")
                       ,textOutput("p_c1")
                       ,hr()
                       ,selectInput(
                         "choicemetrics", "choice metrics",
                         c(hide_metrics = "hide",
                            show_metrics = "show")
                           ),
                       
                      conditionalPanel( 
                        condition = "input.choicemetrics == 'show'",
                        
                                h4("Choice Stream")
                               ,textOutput("value1")
                               ,hr()
                               ,h4("Choice Frequency")
                               ,tableOutput("value2")
                               ,hr()
                               ,h4("Transition Matrix (MC)")
                               ,tableOutput("value3")
                               ,hr()
                               ,h4("Last Choice")
                               ,textOutput("value4")
                               ,hr()
                               ,h4("Conditional Next Choice - MC")
                               ,tableOutput("value5")
                               ,hr()
                               ,h4("Conditional Next Choice - Freq*MC")
                               ,tableOutput("value6")
                               

                        
                               ,h4("Scratch Space")
                               ,textOutput("ran_show")
                               
                               
                      )
                     )),
  server=function(input, output, session) {
    values <- reactiveValues()
    values$df <- data.frame(Column1 = character(0), stringsAsFactors=FALSE)
    values$ran_num <- NULL
    newEntry <- observe({
      if(input$choice1 > 0) {
        #newLine <- isolate(c('input$text1'))
        isolate(values$df[nrow(values$df) + 1,] <- 'choice1')
      }
    })
    newEntry <- observe({
      if(input$choice2 > 0) {
        #newLine <- isolate(c('input$text1'))
        isolate(values$df[nrow(values$df) + 1,] <- 'choice2')
      }
    })
    newEntry <- observe({
      if(input$choice3 > 0) {
        #newLine <- isolate(c('input$text1'))
        isolate(values$df[nrow(values$df) + 1,] <- 'choice3')
      }
    })
    newEntry <- observe({
      if(input$choice4 > 0) {
        #newLine <- isolate(c('input$text1'))
        isolate(values$df[nrow(values$df) + 1,] <- 'choice4')
      }
    })
    rand <- eventReactive(input$choice1,{

      my_rand <- runif(1)
     
      return(my_rand)
    })
    
    #output$table1 <- renderTable({values$df})
    output$value1 <- renderPrint(as.list({values$df}))
    output$value2 <- renderTable(prop.table(table({values$df})))
    output$value3 <- renderTable(as.data.frame(markovchainFit(data=as.character(as.list({values$df})$Column1))$estimate@transitionMatrix))
    output$value4 <- renderText(tail({values$df}[!is.na({values$df})], 1))
    output$value5 <- renderTable(as.data.frame(markovchainFit(data=as.character(as.list({values$df})$Column1))$estimate@transitionMatrix)[tail({values$df}[!is.na({values$df})], 1),])
    output$value6 <- renderTable(
                      as.data.frame(markovchainFit(data=as.character(as.list({values$df})$Column1))$estimate@transitionMatrix)[tail({values$df}[!is.na({values$df})], 1),]
                      *
                      t(prop.table(table({values$df}))))
    output$p_c1 <- renderText(
                    if(nrow(values$df)>3){
                      data.frame(as.data.frame(markovchainFit(data=as.character(as.list({values$df})$Column1))$estimate@transitionMatrix)[tail({values$df}[!is.na({values$df})], 1),]
                          *
                            t(prop.table(table({values$df}))))[,c('choice1')]}else
                              {'Need more info'}
                    )
          
    output$ran_show <- renderText(rand())

    
  }))


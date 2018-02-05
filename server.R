source('functions.R')

server <- function(input, output) {
  
  Report_download <- reactiveValues(
    
    plot_backtest = NULL,
    outtable =NULL,
    finaldata = NULL

      )
  
  stockpage <- eventReactive(input$testnow, {
    
    tradedata <- plot_backtest(input)
    
    Report_download$finaldata <- tradedata$allbuydata
    
  })
  
  output$test_useIndex1 <- renderUI({
    if (input$test_diff_index_strategy == "No" ) return(NULL)
    selectInput("test_useIndex1",h5("Based on"), # 根据/
                choices = c("Stock","Index 1"))
  })
  
  output$test_diff_index <- renderUI({
    if (input$test_diff_index_strategy == "No" ) return(NULL)
    selectInput("test_diff_index",h5( "Index2"),  # 指数2/
                choices = c("None",indexname))
  })
  
  
  output$test_plot1 <- renderPlot({
    
    if (input$testnow == 0) return(NULL)
    isolate({
      
    tradedata <- plot_backtest(input)
    tradedata$plot_backtest
    
    })
  })
  
  output$testtable <- DT::renderDataTable(DT::datatable({
    
    if (input$testnow == 0) return(NULL)
    isolate({
      
    tradedata <- plot_backtest(input)
    table_tradinghistory(tradedata, input)
})
  },rownames = FALSE))

  output$analtable <- renderTable({
    
    if (input$testnow == 0) return(NULL)
    isolate({
    tradedata <- plot_backtest(input)
    table_tradinganalyse(tradedata, input)
    })
    
  },rownames = TRUE)
  
  
  output$downloadSingle <- downloadHandler(
    filename = function() { 
      paste0(input$symbol, "_SingleIndex_" ,Sys.Date(), '.html') 
    },
    content = function(file) {
      
      out = render('single.Rmd', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
      
    },
    contentType = 'application/html'
    
  )
  
  output$downloadMultiframe <- downloadHandler(
    filename = function() { 
      paste0(input$multi_symbol, "_Multiframe_" ,Sys.Date(), '.html') 
    },
    content = function(file) {
      
      out = render('multiframe.Rmd', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
      
    },
    contentType = 'application/html'
    
  )
  
  output$downloadBacktest <- downloadHandler(
    filename = function() { 
      paste0(input$test_stock, "_Backtest_", input$test_scale_plot1 ,"_" ,Sys.Date(), '.html') 
    },
    content = function(file) {
      
      out = render('backtest.Rmd', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
      
    },
    contentType = 'application/html'
    
  )
  

  
  output$Strategytable <- DT::renderDataTable(DT::datatable({
    substra <- substrategydata(strategydata,input)
    showsubstra <- substra[,c( "Stock", "Strategy", "Scale" , "Strategy Return"  ,"Stock Return",
                               "Strategy vs Stock Ratio", "Strategy Annualized Return", 
                               "Stock Annualized Return" ,"Strategy Max Drawdown" , "Stock Max Drawdown",
                               "Total Buy", "Winning Percent")]
    #colnames(substra) <- stracolnames
  },rownames = FALSE))
  
  output$StrategyVariableTable <- renderTable({
    substra <- substrategydata(strategydata,input)
    
    outtable <- t(as.matrix(summary(substra[,input$StrategyPlotVariable]))) 
    row.names(outtable) <- input$StrategyPlotVariable
    outtable
  },rownames = TRUE)
  
  output$StrategyVariablePlot <- renderPlot({
    substra <- substrategydata(strategydata,input)
    
    plotdata <- substra[, c("Stock", input$StrategyPlotVariable)]
    colnames(plotdata)[2]<- "Variable"
    
    
    ggplot(data=substra, aes( plotdata$Variable )) +
             geom_histogram() + xlab(input$StrategyPlotVariable)
    
  })
  
  output$downloaddata <- downloadHandler(
    filename = function() {
      paste("backtest.csv", sep = "")
    },
    content = function(file) {
      write.csv(Report_download$finaldata, file ,row.names = FALSE)
    }
  )
  
  
  
  
  ntext <- eventReactive(input$copytogooglesheet, {
    
    gap_ChineseA <- gs_read(ss,  ws = "ChineseA")
    gap_US <- gs_read(ss,  ws = "US")
    
    substra <- substrategydata(strategydata,input)
    showsubstra <- substra[,c( "Stock", "Strategy", "Scale" , "Strategy Return"  ,"Stock Return",
                               "Strategy vs Stock Ratio", "Strategy Annualized Return", 
                               "Stock Annualized Return" ,"Strategy Max Drawdown" , "Stock Max Drawdown",
                               "Total Buy", "Winning Percent")]
    newdata <- showsubstra[,1:3]
    
    if ( input$market == "ChineseA" ){
      
      gs_ws_delete(ss,ws = "ChineseA")
      ss <- googlesheets::gs_key(sheet_key)
      
      gs_ws_new( ss, ws = "ChineseA")	
      ss <- googlesheets::gs_key(sheet_key)
      
      gs_edit_cells(ss,ws = "ChineseA",  input = newdata)
    } else if (input$market == "US"  ){
      
      gs_ws_delete(ss,ws = "US")
      ss <- googlesheets::gs_key(sheet_key)
      
      gs_ws_new( ss, ws = "US")	
      ss <- googlesheets::gs_key(sheet_key)
      
      
      gs_edit_cells(ss,ws = "US",  input = newdata)
      
    }
    
      
  })
  
  
  
  worksheet <- reactive({
    input$ws
  })
  
  output$mystock <- renderDataTable({
    
    if (input$currentstock == 0) return(NULL)
    
    isolate({
      #gap_ChineseA <- gs_read(ss,  ws = "ChineseA")
      #gap_US <- gs_read(ss,  ws = "US")
      
      gap_Current <- gs_read(ss,  ws = worksheet())
      my_Current <- get_current(gap_Current)
      gs_edit_cells(ss,ws = worksheet(),  input = my_Current)
      
      #my_ChineseA <- get_current(gap_ChineseA)
      #my_US <- get_current(gap_US)
      
      #gs_edit_cells(ss,ws = "ChineseA",  input = my_ChineseA)
      #gs_edit_cells(ss,ws = "US",  input = my_US)

    })
    
    datatable(my_Current)
    
  })
  
  
  
}
source('functions.R')

ui <- shinyUI(navbarPage("Stock Analysis", #  / 趋势的趋势
                         
                         tabPanel("Back Test", # 回测/
                                  fluidPage(
                                    
                                    fluidRow(
                                      # add table here
                                      tableOutput("analtable"), 
                                      
                                      hr(),
                                      
                                      column(3,
                                            
                                             downloadButton('downloadBacktest', "Download Report"),
                                             downloadButton('downloaddata', "Download Data"),
                                             
                                             hr(),
                                             
                                             selectInput("test_stock", h5("Stock"), choices =c( "SPX")), # 股票/
                                             selectInput("test_target", h5("Benchmark"), choices = c( "SPX")), # 基准股票/
                                             
                                             dateRangeInput("test_dates", 
                                                            h5("Analysis Date range"), # 分析时间/
                                                            start = as.character("1950-01-03" ), 
                                                            end = as.character("2017-12-22") , 
                                                            max = "2017-12-31", min = "1950-01-01"),
                                             selectInput("test_scale_plot1", h5("Scale"), #级别/
                                                         choices = c("Daily", "Weekly", "Monthly"), 
                                                         selected = "Daily"),
                                             selectInput("test_transform", h5("Transformation"),  # 变换/
                                                         choices = c("Normal", "Log-Transform")),
                                             selectInput("test_direction", h5("Open"),  # 开仓/
                                                         choices = c("Buy Only", "Short Only","Both")),
                                             numericInput("stoploss", 
                                                          label = h5("Stoploss(%)"),  # 止损/
                                                          value = 0) , 
                                             numericInput("test_rf", 
                                                          label = h5("Risk Free(%)"),  # 无风险利率/
                                                          value = 4) , 
                                             
                                             
                                             tags$div(class="header", checked=NA,
                                                      tags$p("Data Source: Yahoo Finance"),
                                                      tags$a(href="http://www.MinchunZhou.com", "By Minchun Zhou")
                                             )
                                      ),

                                      column(6,
                                             
                                             actionButton("testnow", "Test Now!"),
                                             
                                             plotOutput("test_plot1"),#, hover = "test_hover"),
                                             
                                             hr(),
                                             
                                             DT::dataTableOutput("testtable")
                                             
                                             # zoom in           
                                             #dblclick = "plot1_dblclick",
                                             #brush = brushOpts(
                                             #  id = "plot1_brush",
                                             #  resetOnNew = TRUE)),
                                             
                                      ),
                                      
                                      column(3,
                                             h4("Criteria1"), # 标准1/
                                             
                                             selectInput("test_index", h5("Index1"),  # 指数1/
                                                         choices = c("MACD", "Stock", indexname)),
                                             selectInput("test_differential",h5( "Differentiation"),  #导数/
                                                         choices = c("None","1st", "2nd","3rd")), 
                                             hr(),
                                             
                                             h4("Criteria2"), # 标准2/
                                             selectInput("test_diff_index_strategy", h5("Algorithm using Criteria2"),  # 策略是否使用指标2/
                                                         choices = c("Yes", "No"), selected = "No"),
                                             
                                             uiOutput("test_useIndex1"),
                                             uiOutput("test_diff_index"),
                                             
                                             
                                             hr(),
                                             
                                             numericInput("test_indexscale", 
                                                          label = h5("Index Scale"),  # 指数倍数/
                                                          value = 10),
                                             numericInput("test_num", 
                                                          label = h5("Index Threhold"),  # 指数临界值/
                                                          value = 0),
                                             numericInput("test_open", 
                                                          label = h5("Buy/Short Consecutive"),  # 买入/卖空连续 / 
                                                          value = 1)  ,
                                             numericInput("test_close", 
                                                          label = h5("Sell/Cover Consecutive"),  # 卖出/回买连续 / 
                                                          value = 1)   
                                      )
                                    )    
                                  )
                         )
                         
))

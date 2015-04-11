library(shiny)
library(shinydashboard)
library(caTools)

dashboardPage(
  dashboardHeader(
    title="Wolverine"
    #     disable=T
  ),
  dashboardSidebar(
    #     disable = TRUE
    htmlOutput("getStatsText", align='center'),
    
    selectInput(inputId="asset.type",label="Select Asset", choices=c(#"Gold/Silver 1 min",
                                                                     "Gold/Silver 10 min",
                                                                     "Gold/Silver 15 min",
                                                                     "Gold/Silver 60min",
                                                                     #"DSX / EP 10 min",
                                                                     "NKD/Yen 60 min",
                                                                     "SHFE AU/AG 5 min"
    )),
    br(),
    fluidRow(actionButton("generate",  "Run Test", icon=icon("play", lib="glyphicon")) ,align="center"),
    br(),
    numericInput(inputId="x.size",label="Set X Size",min=1, max=20,value=1,step=1),
    numericInput(inputId="y.size",label="Set Y Size",min=1, max=20,value=1,step=1),
    numericInput(inputId="period.x", label = "Period (x)", min = 5, max = 100, value = 50, step = 1),
    numericInput(inputId="thres", label="Set S1S2 Threshold", min=0, max = 1, value=0.3, step=0.01),
    sliderInput(inputId="stdev.value", label = "Set Std Dev", min = 0.1, max = 3, value = 2),
    checkboxGroupInput(inputId="signal.type", label = "Set Signal Type",choices=c("s1s2","zScore BBand"), selected=c("s1s2"))
    
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plotEquityCurve"), width=12)
    ),
    fluidRow(
      box(plotOutput("plotGS"), width=12)
    ),
    fluidRow(
      downloadButton('downloadData', 'Download'), align="center"
    ),br(),
    fluidRow(
      box(dataTableOutput("getTable"), width=12)
    )
  ) 
)

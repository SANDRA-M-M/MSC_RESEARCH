
library(shinyWidgets)
library(shinycssloaders)

ui <- fluidPage(
  
  ######## BACKGROUND
  setBackgroundColor(
    color = c("#c7fa91", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  #######
  
  titlePanel(tags$div(tags$b(
    'CHI-SQUARE TEST',
    style="color:#000000"))),
  
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)",
              accept=c("text/csv", 
                       "text/comma-separated-values,
                       text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    selectInput(
      "method", "Choose the your application",
      c(
        Goodness_of_Fit = "gof",
        Independence_of_Attribute = "indat"
      ),
      "indat"
    ),
 uiOutput('var'),
 
    tags$br(),
    conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
    tags$br(),
    h5(
      tags$div(
        tags$br(),
        "Developed by:",
        tags$br(),
        tags$br(),
        tags$b("Dr.Pratheesh P. Gopinath"),
        tags$br(),
        tags$b("Assistant Professor,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$b("Sandra M. M."),
        tags$br(),
        tags$b("M.Sc. Agricultural Statistics"),
        tags$br(),
        tags$br(),
        "post your queries at: pratheesh.pg@kau.in"
        ,style="color:#343aeb")
    )
    ),
    mainPanel(
      conditionalPanel("$('#result1').hasClass('recalculating')",
                       tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
      htmlOutput('hyp1'),
      tags$br(),
      tags$br(),
      tableOutput('result1'),
      tags$br(),
      tags$br(),
      htmlOutput('note1'),
      tags$br(),
      tags$br(),
      htmlOutput('hyp2'),
      tags$br(),
      tags$br(),
      tableOutput('result2'),
      tags$br(),
      tags$br(),
      htmlOutput('note2'),
      tags$br(),
      tags$br(),
      tableOutput('result3'),
      tags$br(),
      tags$br(),
      tableOutput('result4'),
      tags$br(),
      tags$br(),
      tags$style(  type="text/css", "#result1 th,td {border: medium solid #000000;text-align:center}"),
      tags$style(  type="text/css", "#result1 td {border: medium solid #000000;text-align:center}"),
      htmlOutput('text1'),
      htmlOutput('text2')
    )
  )


library(shinyWidgets)
library(shinycssloaders)




ui <- fluidPage(
  
  ######## BACKGROUND
  setBackgroundColor(
    color = c("#ecff9e", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  #######
  
  titlePanel(tags$div(tags$b(
    'Index construction',
    style="color:#000000"))),
  
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)",
              accept=c("text/csv", 
                       "text/comma-separated-values,
                       text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    
    uiOutput('var'),
    tags$br(),
    conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
    tags$br(),
    h5(
      tags$div(
        tags$br(),
        "Developed by:",
        tags$b(""),
        tags$b("Sandra M. M."),
        tags$br(),
        tags$b("M.Sc. Agricultural Statistics"),
        tags$br(),
        tags$br(),
        tags$b("Dr.Brigit Joseph"),
        tags$br(),
        tags$b("Professor and Head,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
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
        tags$br(),
        
        tags$br(),
        tags$br(),
        
        "post your queries at: pratheesh.pg@kau.in"
        ,style="color:#343aeb")
    )
    ),
    mainPanel(
      
      

      
      conditionalPanel("$('#result1').hasClass('recalculating')",
                       tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
      htmlOutput('text'),
      htmlOutput('text1'),
      uiOutput("num_inputs"),
      tableOutput('index'),
      tableOutput('tab'),
      htmlOutput('note'),
      tableOutput('result1'),
      tags$style(  type="text/css", "#result1 th,td {border: medium solid #000000;text-align:center}"),
      tags$style(  type="text/css", "#result1 td {border: medium solid #000000;text-align:center}"),
      
    )
  )

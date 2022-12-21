
######## List of required packages for appearance

library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(shinycssloaders)

################################################## Ui
ui <- fluidPage(
  # Background colour
  setBackgroundColor(
    color = c("#fc97a8", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  # Title Panel and colour
  titlePanel(tags$div(tags$b("Binary Logistic Regression"), style = "color:#000000")),
  
  # Entries in sidebar panel
  
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
    ),
    checkboxInput("header", "Header", TRUE),
    uiOutput("var"),
    tags$br(),
    h5(
      tags$div(
        "Developed by:",
        tags$br( " "),
        tags$b("Dr. Pratheesh P. Gopinath"),
        tags$br(),
        tags$b("Assistant Professor,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$b("Dr. Brigit Joseph"),
        tags$br(),
        tags$b("Professor and Head,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$b("Sandra M.M."),
        tags$br(),
        tags$b("M.Sc. Agricultural Statistics"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        h3(),
        "post your queries at: pratheesh.pg@kau.in",
        style = "color:#343aeb"
      )
    ),
    conditionalPanel(
      "$('#path').hasClass('recalculating')",
      tags$div(tags$b("Loading ...please wait while calculation is going on in the background.....please dont press submit button again "), style = "color:green")
    )
  ),
  mainPanel(
    tabsetPanel(
      type = "tab",
      tabPanel(
        "Analysis.Results",
        conditionalPanel(
          "$('#path').hasClass('recalculating')",
          tags$div(tags$b("Loading ...please wait while calculation is going on in the background.....please dont press submit button again "), style = "color:green")
        ),
        uiOutput("note1"),
        tags$br(),
        tableOutput("result1"),
        tags$br(),
        tableOutput("note3"),
        tags$br(),
        tableOutput("result2"),
        tags$br(),
        tags$br(),
        tags$br()
      ),
      tabPanel(
        "Figures",
        tags$br(),
        tags$br()
      )
    )
  )
)

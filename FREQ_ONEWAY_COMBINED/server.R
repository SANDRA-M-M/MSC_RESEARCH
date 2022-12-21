
library(shiny)
library(shinyWidgets)
library(shinycssloaders)

server <- function(input, output, session) {
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return(NULL)
    }
    dt <- read.csv(csvfile$datapath, header = input$header, sep = ",")
    dt
  })

  ################ appear after uploading file
  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (input$req1 == "mean&sd") {
      list(
        radioButtons("obs",
          "Please pick the column for which you need to construct the frequency table
                     ",
          choices = names(csvfile())
        ),
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    } 
    else if (input$req1 == "given_range") {
      list(
        radioButtons("obs",
          "Please pick the column for which you need to construct the frequency table
                     ",
          choices = names(csvfile())
        ),
        numericInput("num1",
          h3("lower limit of the medium class"),
          value = 1
        ),
        numericInput("num2",
          h3("upper limit of the medium class"),
          value = 1
        ),
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })


  ##################################### mean &sd table
  output$meansd <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        input$reload
        Sys.sleep(2)
        d <- as.data.frame(csvfile())
        obs <- d[, input$obs]
        data <- as.numeric(gsub(",", "", obs))

        sd <- sd(data)

        m <- mean(data)
        l <- m - sd
        h <- m + sd
        table <- cbind(sd, l, m, h)
        colnames(table) <- c("sd", "mean -sd", "mean", "mean +sd")
        table
      }
    },
    digits = 3,
    caption = ("<b> standard deviation and mean </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  #####################################  freq table
  output$result1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "mean&sd") {
        if (input$submit > 0) {
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          obs <- d[, input$obs]
          data <- as.numeric(gsub(",", "", obs))
          m <- mean(data)
          sd <- sd(data)
          l <- m - sd
          h <- m + sd
          low <- ifelse(data < l, "low", "")
          high <- ifelse(data > h, "high", "")
          medium <- ifelse(data >= l & data <= h, "medium", "")
          category <- paste(low, medium, high)
          table(category)
          table <- as.data.frame(table(category))

          percentage <- round(100 * table$Freq / length(data), 2)

          table <- cbind(table, percentage)

          table
        }
      }
    },
    digits = 3,
    caption = ("<b> One way frequency table for classification based on mean and sd criteria </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )


  ### range
  output$range <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "given_range") {
        if (input$submit > 0) {
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          obs <- d[, input$obs]
          data <- as.numeric(gsub(",", "", obs))
          m <- mean(data)
          sd <- sd(data)
          l <- input$num1
          h <- input$num2
          low <- ifelse(data < l, "low", "")
          high <- ifelse(data > h, "high", "")
          medium <- ifelse(data >= l & data <= h, "medium", "")
          category <- paste(low, medium, high)
          table(category)
          table <- as.data.frame(table(category))

          percentage <- round(100 * table$Freq / length(data), 2)

          table <- cbind(table, percentage)

          table
        }
      }
    },
    digits = 3,
    caption = ("<b> One way frequency table based on given range criteria</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

}

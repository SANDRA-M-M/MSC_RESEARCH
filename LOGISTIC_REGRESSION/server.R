
######## List of required packages for the tool

library(ggplot2)
library(GGally)
library(CCA)
library(CCP)
library(Hmisc)

############################################### server
server <- function(input, output, session) {
  # file uploading
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return()
    }
    
    # Read the uploaded file
    dt <- read.csv(csvfile$datapath,
                   header = input$header,
                   sep = ","
    )
    dt
  })
  
  # After reading the file the variables appear
  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    colnames <- reactive({
      names(csvfile())
    })
    
    observeEvent(csvfile(), {
      updateCheckboxGroupInput(session, "X",
                               choices = colnames(),
                               selected = FALSE
      )
    })
    
    inputVar <- reactive({
      Fixedvar <- input$X
      
      if (setequal(colnames(), Fixedvar)) {
        # If sets are equal, return an empty string
        return("")
      } else {
        # Return elements that are only in colnames
        setdiff(colnames(), Fixedvar)
      }
    })
    
    observeEvent(inputVar(), {
      updateCheckboxGroupInput(session, "Y", choices = inputVar())
    })
    list(
      # first checker box input
      checkboxGroupInput(
        "X",
        "Please select the binary dependent variable ",
        choices = names(csvfile()) # Choices should be the column names
      ),
      # second checker box input
      checkboxGroupInput(
        "Y",
        "Please select the independent variables ",
        choices = names(csvfile()) # Choices should be the column names
      ),
      actionBttn(
        inputId = "submit",
        label = "Run Analysis!",
        color = "danger",
        style = "jelly"
      )
    )
  })
  
  ############################### This note appear on opening
  output$note1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(HTML(
        paste0(
          " <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Values of variables should be entered as columns </li>
<li>Variable names should be given as column headings </li>
<li>Don't type or delete anything on other cells without data</li>
<li>You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Variable name’ should be written as Variable_name or Variable.name</li>
<li>Short names may be selected for column name as it may look good in path diagram</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank </li>
<li>Type 'NA' in the cell with no observation</li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<h5> You can download a model data set from below and test the App  </h5>
"
)))
    } else {
      return()
    }
  })
  


  
#to show after the coefficients table 
  output$note3 <- renderUI(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {

        return(HTML(
          paste0(
            "
<ui>
<li><b>Interpretation:If p-value is less than 0.05, the coefficient is significant.</li>
<li><b>If the sign is positive, one unit increase in each independnt variable will cause the coefficient times increase in the log of odds of success of the dependent variable.</li>
</ui>
</p>

"
          )
        )) 
      }
    }
 
  )  
  
  
  
  
  
  ##  table of coefficients
  output$result1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        X <- as.data.frame(subset(csvfile(), select = input$X))
        Y <- as.data.frame(subset(csvfile(), select = input$Y))
        form <- sprintf("%s~%s",input$X,paste0(input$Y,collapse="+"))
        z<-cbind(X,Y)
        model<-glm(as.formula(form),data<-z,family = "binomial")
        som <-summary(model)
        coef<-som$coefficients[,1]
        stde<-som$coefficients[,2]
        pval<-som$coefficients[,4]
        tab<-cbind(coef,stde,pval)
        colnames(tab)<- c("coefficients","standard error","p-value")
        tab
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Regression Coefficients</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )  
  
  
  
  
  #miscellaneous
  output$result2 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
      
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Regression Coefficients</b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4
  )  
  
  
  ####################################################### Details about model dataset
  
  
  
  
  
}

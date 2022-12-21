
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(ltm)

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
    if (input$req1 == "split_half") {
      list(
        
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    } 
    else if (input$req1 == "cronbach") {
      list(
        
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    
    
    else if (input$req1 == "test_retest") {
      list(
        checkboxGroupInput("test", "Please select the columns of responses for first administration", choices = names(csvfile())),
        checkboxGroupInput("retest", "Please select the columns of responses for second administration", choices = names(csvfile())),
        
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })

  
  
  
  
  ############################### this note appear on opening
  output$note <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(
        HTML(paste0(" <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Observations should be pasted as columns </li>
<li>Don't type or delete anything on other cells without data</li>
<li>You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Variable name’ should be written as Variable_name or Variable.name</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank </li>
<li>Type 'NA' in the cell with no observation</li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<h4> This includes three methods:</h4>
<li>Spearman-Brown Prophecy formula </li>
<li> Cronbach's Alpha and</li>
<li> Test-retest Reliability</li>
<li>For the first two methods you only need to apply the file in csv format </li>
<li> For test-retest reliability Please carefully chooose the columns of first test results in the first checkbox set and the second test results in the second checkbox set</li>
<li> </li>
<li> </li>
<p>

</p>
<h5> You can download a model data set from below and test the App  </h5>
"))
      )
    }
    
    else {
      return()
    }
  })

  #####################################cronbach's alpha
  output$cronbach <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "cronbach"){
      if (input$submit > 0) {
        input$reload
        Sys.sleep(2)
        d <- as.data.frame(csvfile())
        v <-cronbach.alpha(d)
        respondents<-v$n
        items <-v$p
        alpha <- v$alpha

       
        table <- cbind(respondents,items,alpha)
        table
      }
      }
    },
    digits = 3,
    caption = ("<b> Cronbachs Alpha </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  #####################################split
  output$split <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "split_half") {
        if (input$submit > 0) {
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          even <- as.matrix(d[,c(FALSE,TRUE)])
          evens <- rowSums(even)
          
          odd <- as.matrix(d[,c(TRUE,FALSE)])
          odds <- rowSums(odd)
          
          #product-moment correlation
          test <- cor.test(odds,evens)
          
          #results of test
          RCH <- test$estimate
          tstat <- test$statistic
          pval <- test$p.value
          RC <- 2*RCH/(1+RCH)
          table <- cbind(RCH,tstat,pval,RC)
          colnames(table)<- c("correlation","t-value","p-value","reliability coeficient")
          table
        }
      }
    },
    digits = 3,
    caption = ("<b> Split half reliability and Spearman Brown prophesy formula </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )


  
  
  
  

  
  
  ### range
  output$testret <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "test_retest") {
        if (input$submit > 0) {
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          
          x <- as.data.frame(subset(csvfile(), select = input$test))
          y <- as.data.frame(subset(csvfile(), select = input$retest))
          a <- apply(x, MARGIN = 1,FUN = sum)
          b <- apply(y, MARGIN = 1,FUN = sum)
          test<- cor.test(a,b, method = "pearson")
          c<- test$estimate
          t <- round(test$statistic,4)
          df <-test$parameter         
          p<- test$p.value
          table <- cbind(c,t,df,p)
colnames(table)<-c("correlation","t-statistic","degrees of freedom","p-value")
          table
        }
      }
    },
    digits = 3,
    caption = ("<b> Test-retest reliability</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  


  
  ############################### this note appear on opening
  output$note2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()}
      if (is.null(input$submit)) {
        return()
      }
      
      if (input$req1 == "test_retest"){
        if (input$submit > 0) {
      
        HTML(paste0(" <h4> Reliability is described by positive and significant correlation. </h4>
                    <p><h4>If the p-value is less than 0.05 the correlation is significant with 95 percent confidence.</h4></p>"))
         
      }
    }

    
  }
  
  )
  
  
  
  
    
}

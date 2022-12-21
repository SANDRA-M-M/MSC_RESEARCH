
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
    
    if (is.null(input$method)) {
      return()
    }
    
    if (input$method == "gof") {
      list(
        radioButtons("obs", "Please pick observed Variable", choices = names(csvfile())),
        
        radioButtons("exp", "Please pick expected Variable", choices = names(csvfile())),
     
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    
   else if (input$method == "indat") {
      list(
        radioButtons("first", "Please pick first Variable", choices = names(csvfile())),
        
       
        
        radioButtons("second", "Please pick second Variable", choices = names(csvfile())),
        
        
        
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
   } 
   
    
  })

    

  
  ##################################### chi-square result for goodness of fit
  output$result1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        if(input$method == "gof" ){
         
            
        validate(
          need(input$obs != input$exp, "Warning : 
               Please select observation and 
               expected value columns properly")
        )
        input$reload
        Sys.sleep(2)
        d <- as.data.frame(csvfile())
        obs <- d[, input$obs]
        exp <- d[, input$exp]
        l<- sum(exp)
        exp<- exp/l
                 test<-chisq.test(obs,p=exp
                                  )
                 
                 chi<-test$statistic
                 df<-test$parameter
                 pval<-test$p.value
                 final <- cbind(chi,df,pval)
                 colnames(final)<-c("Chi-square","Degrees of Freedom", "P-value")
             final
        
      }  
        
      }
    },
    digits = 3,
    caption = ("<b> Chi-Square test for goodness of fit </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  ) 
  
  #################################### note for goodness of fit
  output$note1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    
      if(input$method == "gof"){
        if (input$submit > 0) {
          
      validate(
        need(input$obs != input$exp, "")
      )
          
      d <- as.data.frame(csvfile())
      obs <- d[, input$obs]
      exp <- d[, input$exp]
      l<- sum(exp)
      exp<- exp/l
      test<-chisq.test(obs,p=exp,simulate.p.value = T)
      chi<-test$statistic
      df<-test$parameter
      pval<-test$p.value
      final <- cbind(chi,df,pval)
      colnames(final)<-c("Chi-square","Degrees of Freedom", "P-value")
      
      if (final[1,3] <= 0.05) {
        HTML(paste0("<b>", "Since the P-value is < 0.05, there is a significant difference between the observed and the expected value.", "</b>"))
      }
      else {
        HTML(paste0("<b>", "Since the P-value is > 0.05, there is no significant difference between the observed and the expected value.","</b>"))
      }
    }
    }
  })  
  
  
  
  #################################### hypothesis for goodness of fit
  output$hyp1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      if(input$method =="gof"){
        
        validate(
          need(input$obs != input$exp, "")
        )

        HTML(paste0(" <h4> The hypotheses for the test of goodness of fit is given by:  </h4>
<p>
<ui>
<li>Null Hypothesis: No significant difference between the observed and the expected values </li>
<li>Alternate Hypothesis: there is a significant difference between the observed and the expected values</li>
"))
      }
    }
    else{return()}
  })
  
  
  
  ##################################### chi-square result for independence of attributes
  output$result2 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      } 
      if (input$submit > 0) { 
        if(input$method == "indat" ){  
      validate    
      (need(input$first != input$second, "Warning : 
               Please select observation and 
               expected value columns properly")
      )
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          first <- d[, input$first]
          second <- d[, input$second]
          test<-chisq.test(first,second
          )
          
          chi<-test$statistic
          df<-test$parameter
          pval<-test$p.value
          final <- cbind(chi,df,pval)
          colnames(final)<-c("Chi-square","Degrees of Freedom", "P-value")
          final
                
            

        
        
        
        
        
  
          
          
        } 
          
 
          
           
          
        }  
        
     
    },
    digits = 3,
    caption = ("<b> Chi-Square test for independence of attributes </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  ) 
  
  
  
  
  
  ##################################### observed table for independence of attributes
  output$result3 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        if(input$method == "indat" ){
          
           
          validate(
            need(input$first != input$second, "Warning : 
               Please select observation and 
               expected value columns properly")
          )
         
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          first <- d[, input$first]
          second <- d[, input$second]
          test<-chisq.test(first,second
          )
          test$observed
        
        }  
        
      }
    },
    digits = 3,
    caption = ("<b> Table of observed frequencies </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  ) 
  
  
  
  
  
  ##################################### expected table for independence of attributes
  output$result4 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        if(input$method == "indat" ){
          
          
          validate(
            need(input$first != input$second, "Warning : 
               Please select observation and 
               expected value columns properly")
          )
         
          input$reload
          Sys.sleep(2)
          d <- as.data.frame(csvfile())
          first <- d[, input$first]
          second <- d[, input$second]
          test<-chisq.test(first,second
          )
          tab2<-test$expected
          x<- as.factor(first)
          y<-levels(x)
          tab<-cbind(y,round(tab2,3))
          tab
        
        }  
        
      }
    },
    digits = 3,
    caption = ("<b> Table of expected frequencies </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  ) 
  
  
  
  #################################### note for independence of attributes
  output$note2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      if(input$method =="indat"){
        
        validate(
          need(input$first != input$second, "")
        )
        d <- as.data.frame(csvfile())
        first <- d[, input$first]
        second <- d[, input$second]
        test<-chisq.test(first,second)
        chi<-test$statistic
        df<-test$parameter
        pval<-test$p.value
        final <- cbind(chi,df,pval)
        colnames(final)<-c("Chi-square","Degrees of Freedom", "P-value")
        final
        if (final[1, 3] <= 0.05) {
          HTML(paste0("<b>", "Since the P-value is < 0.05, the two variables are related. ", "</b>"))
        }
        else {
          HTML(paste0("<b>", "Since the P-value is > 0.05, the two variables are independent. "))
        }
      }
    }
  })
  
  
  #################################### hypothesis for independence of attributes
  output$hyp2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      if(input$method =="indat"){
        
        validate(
          need(input$first != input$second, "")
        )
        
        HTML(paste0(" <h4> The hypotheses for the test of independence of attributes is given by:  </h4>
<p>
<ui>
<li>Null Hypothesis: The two variables are independent </li>
<li>Alternate Hypothesis: The two variables are related </li>
"))
        
        
      }
    }
  })
  
  output$note3 <- renderUI({})
                          
  output$note4 <- renderUI({})
  
  
  
}
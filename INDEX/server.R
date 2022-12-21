
library(shiny)
library(shinyWidgets)
library(shinycssloaders)

scaled<- function(x){
  y<- (x-min(x))/(max(x)-min(x))
  y
}
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
   else {
      list(
        checkboxGroupInput("respond","Your respondents column in the uploaded file",choices=names(csvfile()[1])),
        checkboxGroupInput("selvar", "Please check all the columns of indicators to confirm", choices = names(csvfile()[-1])),
       
      
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "pill"
        )
      )
    }
  })





# note to appear on top if the weights are assigned equal

output$text1<- renderText(
  if (is.null(input$file1$datapath)) {
    return()}
  else{
    
    if (input$submit > 0) {
      

        'The indicator is calculated  after standardising each dimension using the formula z =(x- min(x)/[max(x)-min(x)] and with weight of each dimension being 1'
       }} )











##################################### index output

  output$index <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0 ) {
      input$reload
        Sys.sleep(2)
        
     

        
          
        x <- as.data.frame(subset(csvfile(), select = input$selvar))
        y<- as.data.frame(subset(csvfile(), select = input$respond))
        b <- apply(x, MARGIN = 2,FUN = scaled)
        n <- ncol(x)
        wt <- rep(1,n)
        var <- as.matrix(b)
        ind <- as.matrix(wt)
        index <- round(var %*% ind,4)
        table <- cbind(y,index)
        colnames(table) <- c("Respondents","Index")
        table
       
        

        }
    },
    digits = 3,
    caption = ("<b> Index </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

}

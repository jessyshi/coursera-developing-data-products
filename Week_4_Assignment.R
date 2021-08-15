library(shiny)

ui<-fluidPage(
  numericInput(inputId = "weight", value=60, label="Type in your weight (Kg)"),
  numericInput(inputId= "height", value=1.70, label="Type in your height (m)"),
  actionButton(inputId="click", label='Submit'),
  tags$br(),tags$br(),
  textOutput("msg")
)

server<- function(input,output){
  val<-reactiveValues()
  
  observeEvent(input$click,
               {
                 val$bmi <- input$weight / (input$height * input$height)
               })
  
  output$bmi <- renderText({val$bmi})
  
  observeEvent(val$bmi,
               {
                 if(val$bmi<18.5){
                   val$text<-paste("Your bmi is",val$bmi,"which falls within the Underweight category for adults your height")
                 }
                 else if(val$bmi<=24.9){
                   val$text<-paste("Your bmi is",val$bmi,"which falls within the Healthy Weight category for adults your height")
                 }
                 else if(val$bmi<=29.9){
                   val$text<-paste("Your bmi is",val$bmi,"which falls within the Overweight category for adults your height")
                 }
                 else{
                   val$text<-paste("Your bmi is",val$bmi,"which falls within the Obesity category for adults your height")
                 }
               })
  
  output$msg <-renderText({paste('\n',val$text)})
}

shinyApp(ui=ui,server=server)
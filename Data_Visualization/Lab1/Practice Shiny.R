library(shiny)
ui = fluidPage(
  # input functions
  # output functions
)

server = function(input, output){
  #both input and output must appear in your server function and both are list like objects
}

shinyApp(ui = ui, server = server)
 


#inputs
#for slider 
ui <- fluidPage(
  sliderInput(inputId = "num", label = "Choose a number:",
  value = 25, min = 1, max = 100),
  plotOutput("hist")
)
#input_func(inputId = "assign name to input unique", label = "to explain that it does the button", input_specific_arguments)


#outputs
#func_name(outputId = "unique", output_specific_arguments)

#Begin app with basic template
#add elements as arguments to fluidpage()
#create reactive inputs with input functions
#create reactive outputs with output functions
#use the server function to assemble inputs into outputs


#server
server = function(input, output){
  #output$hist = rendrePlot({hist(rnorm(100))}) # rendre function creates the type of output you wish to make
  #input$num gives value set by user to the input slider as input id is num
  output$hist = renderPlot({
    hist(rnorm(input$num))
  })
}
#use server func to assemble inputs into outputs
#save the output that you build to output$
#build the output with a render() func
#access input values with input$
#create reactivity by using inputs to build rendered outputs

shinyApp(ui, server)
#ui object layes out the web page for the app HTML part
#then you write a function for the server to create R components for the app
#then finally the template brings together the UI and the Server to give us a Shiny app


#Reactivity
#What is reactivity?
#changes to the input objects propogates to output objects

#reactive values
#reactive values only work with reactive functions. YOu cannot call a reactive value from outside one
#Reactive values notify the functions that use them to update, reactive functions respond
#reactive values act as the data streams that flow through your app

#reactive functions
#use a chunk of code to build and rebuild an object(WHat code will the function use?)
#the object will respond to changes in a set of reactive values(WHich reactive values will the object respond to?)
#render functions build something that can actually be displayed in UI
#object will respond to every reactive value in the code, 
#when notified by reactive value the object will run the entire block of code associated with it

#Render Functions
#render functions make objects to display
#always save the results to output$
#render makes an observer object that has a block of code associated with it
#the object will rerun the entire block of code to update itself whenever it is invalidated

#App1
ui = fluidPage(
  sliderInput(inputId = "num", label = "Choose a number:", value = 25,
              min = 1, max = 100), 
  plotOutput("hist"),
  verbatimTextOutput("stat")
)

server = function(input, output){
  data = reactive({rnorm(input$num)})
  #reactive expressions knoww if they are valid or invalid,
  #they even save their value for future use
  #reactive expressions cache their value
  #call them like a function
  #saves you computation because of the cache
  
  output$hist = renderPlot(hist(data()))
  
  output$stat = renderPrint(summary(data()))
}

shinyApp(ui, server)


#App2    Text input
#isolate creates values that are not reactive
ui = fluidPage(
  sliderInput(inputId = "num", label = "Choose a number:", value = 25,
              min = 1, max = 100),
  
  textInput(inputId = "title", label = "Write a title:",
            value = "Histogram of random normal values"),
  
  plotOutput("hist"))

server = function(input, output){
  data = reactive({rnorm(input$num)})

  output$hist = renderPlot(hist(data(), 
                           main = isolate(input$title)))
}

shinyApp(ui, server)


#App3     
ui = fluidPage(
  sliderInput(inputId = "num", label = "Choose a number:", value = 25,
              min = 1, max = 100),
  
  textInput(inputId = "title", label = "Write a title:",
            value = "Histogram of random normal values"),
  
  actionButton(inputId = "clicks", label = "Click Me!"),
  
  plotOutput("hist"))

server = function(input, output){
  data = reactive({rnorm(input$num)})
  
  output$hist = renderPlot(hist(data(), 
                                main = isolate(input$title)))
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
}
#action buttons start with a value 0 and increase after every click
#apps should never depend on the actual value of the action button
shinyApp(ui, server)


#observe is parallel to observe event
#observe({print(input$clicks)})   observe will respond to every reactive value in the code
#code block is rerun whenever the observer is invalidated
#observe and observeEvent triggers code that run on the server
#observeEvent you specify precisely which reactive valies should invalidate the observer
#observe is triggered for every reactive value in the code block


#App 4 Delay reactions with eventReactive()
#event reactive is a reactive expression that only responds to specific values
ui = fluidPage(
  sliderInput(inputId = "num", label = "Choose a number:", value = 25,
              min = 1, max = 100),
  
  textInput(inputId = "title", label = "Write a title:",
            value = "Histogram of random normal values"),
  
  actionButton(inputId = "go", label = "Update"),
  
  plotOutput("hist"))

server = function(input, output){
  #data = reactive({rnorm(input$num)})
  data = eventReactive(input$go, { rnorm(input$num)})
  #you can give a single or a vector of reactive values to respond to
  
  output$hist = renderPlot(hist(data(), 
                                main = isolate(input$title)))
}

shinyApp(ui, server)

#App5 Managing state with reactive values
#reactiveValues() creates a list of values to manipulate programmatically
ui = fluidPage(
  actionButton(inputId = "norm", label = "Normal"),
  
  actionButton(inputId = "uni", label = "Uniform"),
  
  plotOutput("hist"))

server = function(input, output){
  rv = reactiveValues(data = rnorm(100))
  
  observeEvent(input$norm, {rv$data = rnorm(100)})
  
  observeEvent(input$uni, {rv$data = runif(100)})
  
  output$hist = renderPlot(hist(rv$data, 
                                main = "Histogram of random data"))
}

shinyApp(ui, server)

#keep in mind what your end product is gonna be
#REDUCE repetition!       place code where it will be rerun as little as necessary
#loading libraries and all place it outside the server function to prevent rerun


#CUSTOMIZING APPEARENCE

library(ggplot2)
library(gridExtra)
library(grid)
library(plotly)
library(shiny)

#Q1
colNames = c("ID", "Length_of_Stay", "Age", "Infection_Risk", "Routine_Culturing_Ratio", 
             "Routine_Chest_X_ray_Ratio", "Number_of_Beds", "Medical_School_Affiliation", 
             "Region", "Average_Daily_Census", "Number_of_Nurses", "Available_Facilities_and_Services")
d = read.table("SENIC.txt", header = F, sep = "", row.names = 1, col.names = colNames)
head(d)

quantiles = function(r){
  q = quantile(r)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  v1 = q3 + 1.5*(q3-q1)
  v2 = q1 - 1.5*(q3-q1)
  u = union(which((r>v1)), which((r<v2)))
  return(u)
}
d$Length_of_Stay[quantiles(d$Length_of_Stay)]

plots = list()
for(i in 1:11){
  plotName = paste("P", i, sep = "")
  colSel = colNames[i+1]
  outliers = quantiles(d[,i])
  if(!identical(outliers, integer(0))){
    df = data.frame(x = d[,i][outliers], y = 0)
    plotOutliers = geom_jitter(data = df, aes(x, y), height = 0, shape = 05, col = "red")
    plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density() + plotOutliers
  }else {
    plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density()
  }
}
grid.arrange(grobs = plots, ncol = 3, nrow = 4,
             top = textGrob("Density plot for all Categories",gp=gpar(fontsize=20,font=3)))

###Try 1
ui <- fluidPage(
  titlePanel(title = "Shiny app for density plots of all categories"),
  plotOutput("plots", hover = hoverOpts(id = "plot_hover", delayType = "throttle")),
  verbatimTextOutput("plot_hoverinfo")
)

server <- function(input, output) {
  colNames = c("Length_of_Stay", "Age", "Infection_Risk", "Routine_Culturing_Ratio", 
               "Routine_Chest_X_ray_Ratio", "Number_of_Beds", "Medical_School_Affiliation", 
               "Region", "Average_Daily_Census", "Number_of_Nurses", "Available_Facilities_and_Services")
  all_plots = list()
  c = 1
  for(i in colNames){
    plotName = paste("P", c, sep = "")
    colSel = i
    outliers = quantiles(d[,i])
    if(!identical(outliers, integer(0))){
      df = data.frame(x = d[,i][outliers], y = 0)
      plotOutliers = geom_jitter(data = df, aes(x, y), height = 0, shape = 05, col = "red")
      all_plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density() + plotOutliers
    }else {
      all_plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density()
    }
    c = c + 1
  }
  
  output$plots <- renderPlot({
    #plots = list()
    #for(i in input$categories){
    #  i = as.integer(i)
    #  plotName = paste("P", i, sep = "")
    #  plots[[plotName]] = all_plots[[plotName]]
    #}
    grid.arrange(grobs = all_plots, ncol = 3, nrow = 4)
  })
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })
}
shinyApp(ui, server)




###Try 2
ui <- fluidPage(
  titlePanel(title = "Shiny app for density plots of all categories"),
  fluidRow(
    column(3, plotOutput("P1",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P2",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P3",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P4",hover = hoverOpts(id = "plot_hover", delayType = "throttle")))
  ),
  fluidRow(
    column(3, plotOutput("P5",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P6",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P7",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(3, plotOutput("P8",hover = hoverOpts(id = "plot_hover", delayType = "throttle")))
  ),
  fluidRow(
    column(4, plotOutput("P9",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(4, plotOutput("P10",hover = hoverOpts(id = "plot_hover", delayType = "throttle"))),
    column(4, plotOutput("P11",hover = hoverOpts(id = "plot_hover", delayType = "throttle")))
  ),
  fluidRow(
    column(6, verbatimTextOutput("plot_hoverinfo")),
    column(6, verbatimTextOutput("P1_hov"))
  )
)

server <- function(input, output) {
  print("Are you working?????")
  colNames = c("Length_of_Stay", "Age", "Infection_Risk", "Routine_Culturing_Ratio", 
               "Routine_Chest_X_ray_Ratio", "Number_of_Beds", "Medical_School_Affiliation", 
               "Region", "Average_Daily_Census", "Number_of_Nurses", "Available_Facilities_and_Services")
  all_plots = list()
  c = 1
  for(i in colNames){
    plotName = paste("P", c, sep = "")
    colSel = i
    outliers = quantiles(d[,i])
    if(!identical(outliers, integer(0))){
      df = data.frame(x = d[,i][outliers], y = 0)
      plotOutliers = geom_jitter(data = df, aes(x, y), height = 0, shape = 05, col = "red")
      all_plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density() + plotOutliers
    }else {
      all_plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density()
    }
    c = c + 1
  }
  output$P1 <- renderPlot({plotly(all_plots$P1)})
  output$P2 <- renderPlot({all_plots$P2})
  output$P3 <- renderPlot({all_plots$P3})
  output$P4 <- renderPlot({all_plots$P4})
  output$P5 <- renderPlot({all_plots$P5})
  output$P6 <- renderPlot({all_plots$P6})
  output$P7 <- renderPlot({all_plots$P7})
  output$P8 <- renderPlot({all_plots$P8})
  output$P9 <- renderPlot({all_plots$P9})
  output$P10 <- renderPlot({all_plots$P10})
  output$P11 <- renderPlot({all_plots$P11})
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })
  
  output$P1_hov <- renderPrint({
    if(!is.null(input$plot_hover)){
      print(input$plot_hover)
      hover=input$plot_hover
      dist=sqrt((hover$x-d$Length_of_Stay)^2+(hover$y-d$Length_of_Stay)^2)
      if(min(dist) < 3){
        id = which.min(dist)
        print(id)
        if(id %in% outliers(d$Length_of_Stay))
          id
      }
    }
  })
}
shinyApp(ui, server)




##Examples
shinyApp(
  ui = basicPage(
    fluidRow(
      column(width = 4,
             plotOutput("plot", height=300,
                        click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                        hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                        brush = brushOpts(id = "plot_brush")
             ),
             h4("Clicked points"),
             tableOutput("plot_clickedpoints"),
             h4("Brushed points"),
             tableOutput("plot_brushedpoints")
      ),
      column(width = 4,
             verbatimTextOutput("plot_clickinfo"),
             verbatimTextOutput("plot_hoverinfo")
      ),
      column(width = 4,
             wellPanel(actionButton("newplot", "New plot")),
             verbatimTextOutput("plot_brushinfo")
      )
    )
  ),
  server = function(input, output, session) {
    data <- reactive({
      input$newplot
      # Add a little noise to the cars data so the points move
      cars + rnorm(nrow(cars))
    })
    output$plot <- renderPlot({
      d <- data()
      plot(d$speed, d$dist)
    })
    output$plot_clickinfo <- renderPrint({
      cat("Click:\n")
      str(input$plot_click)
    })
    output$plot_hoverinfo <- renderPrint({
      cat("Hover (throttled):\n")
      str(input$plot_hover)
    })
    output$plot_brushinfo <- renderPrint({
      cat("Brush (debounced):\n")
      str(input$plot_brush)
    })
    output$plot_clickedpoints <- renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(data(), input$plot_click, "speed", "dist")
      if (nrow(res) == 0)
        return()
      res
    })
    output$plot_brushedpoints <- renderTable({
      res <- brushedPoints(data(), input$plot_brush, "speed", "dist")
      if (nrow(res) == 0)
        return()
      res
    })
  }
)


##ANother example

m = mtcars
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 350,hover = hoverOpts(id ="plot_hover"))
    )
  ),
  fluidRow(
    column(width = 5,
           verbatimTextOutput("hover_info")
    )
  )
)

server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    
    ggplot(mtcars, aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
    
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-mtcars$mpg)^2+(hover$y-mtcars$disp)^2)
      cat("Weight (lb/1000)\n")
      if(min(dist) < 3){
        print(which.min(dist))
        print("is this true!")
        mtcars$wt[which.min(dist)]
      }
    }
  })
}
shinyApp(ui, server)


##ANother try

m = mtcars
ui <- fluidPage(
  fluidRow(
    column(width = 8, plotOutput("plot1", height = 350,hover = hoverOpts(id ="plot_hover"))),
    column(width = 4, verbatimTextOutput("hover_info"))
    )
)

server <- function(input, output) {
  
  outliers_rv = reactiveVal()
  df = data.frame(x = d$Infection_Risk[quantiles(d$Infection_Risk)], y = 0, z = "outs")
  outliers_rv(df)
  
  output$plot1 <- renderPlot({
    plotOutliers = geom_jitter(data = outliers_rv(), aes(x, y, col = z), height = 0, shape = 05)
    ggplot(d, aes(Infection_Risk)) + geom_density() + plotOutliers
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist= sqrt((hover$x-d$Infection_Risk[quantiles(d$Infection_Risk)])^2)
      if(min(dist) < 0.2){
        print(which.min(dist))
        ind = which.min(dist)
        df = outliers_rv()
        df$z = as.character(df$z)
        df[ind, 'z'] = 'phigh'
        df$z = as.factor(df$z)
        outliers_rv(df)
        d[which.min(dist), ]
      }else {
        df = outliers_rv()
        df$z = "outs"
        outliers_rv(df)
      }
    }
  })
}
shinyApp(ui, server)

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

#Q2
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
quantiles(d$Infection_Risk)

#Q3
q = quantiles(d$Infection_Risk)
plotQ3 = ggplot(d, aes(Infection_Risk)) + geom_density() +
  geom_jitter(data = data.frame(x = d$Infection_Risk[q], y = 0), aes(x, y), height = 0, shape = 5, col = "red")
plotQ3

#Q4
plots = list()
for(i in 1:11){
  plotName = paste("P", i, sep = "")
  colSel = colNames[i+1]
  outliers = quantiles(d[,i])
  if(!identical(outliers, integer(0))){
    df = data.frame(x = d[,i][outliers], y = 0)
    plotOutliers = geom_jitter(data = df, aes(x, y), height = 0, shape = 05)
    plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density() + plotOutliers
  }else {
    plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density()
  }
}
grid.arrange(grobs = plots, ncol = 3, nrow = 4)

#Q5
a = ggplot(d, aes(Infection_Risk, Number_of_Nurses, col = Number_of_Beds)) + geom_point()
a

#Q6
ggplotly(plotQ3)

#Q7
outliers_inf = d$Infection_Risk[quantiles(d$Infection_Risk)]
plot_ly(x = d$Infection_Risk, name = 'Infection Risk') %>% add_histogram(name = 'plotly.js') %>%
  add_trace(x = ~outliers_inf,y=0, name = 'outliers', type = 'scatter', mode = 'markers', symbol=I(05))


#Q8
ui <- fluidPage(
  titlePanel(title = "Shiny app for density plots of all categories"),
  fluidRow(
    column(4, 
    sliderInput("bw", "Bandwidth parameter:",
                min = 0.1, max = 5, value = 1
    ),
    checkboxGroupInput("categories", "Categories to show:",
                       c("Length of Stay" = "Length_of_Stay", 
                         "Age" = "Age",
                         "Infection Risk" = "Infection_Risk",
                         "Routine Culturing Ratio" = "Routine_Culturing_Ratio", 
                         "Routine Chest X-ray Ratio" = "Routine_Chest_X_ray_Ratio",
                         "Number of Beds" = "Number_of_Beds",
                         "Medical School Affiliation" = "Medical_School_Affiliation",
                         "Region" = "Region",
                         "Average Daily Census" = "Average_Daily_Census",
                         "Number of Nurses" = "Number_of_Nurses",
                         "Available Facilities and Services" = "Available_Facilities_and_Services"),
                       selected = "Infection_Risk")
    ),
    column(8, plotOutput("plots"))
  )
)


server <- function(input, output) {
  output$plots <- renderPlot({
    plots = list()
    for(i in input$categories){
      plotName = paste("P", i, sep = "")
      colSel = i
      outliers = quantiles(d[,i])
      if(!identical(outliers, integer(0))){
        df = data.frame(x = d[,i][outliers], y = 0)
        plotOutliers = geom_jitter(data = df, aes(x, y), height = 0, shape = 05, col = "red")
        plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density(bw = input$bw) + plotOutliers
      }else {
        plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density(bw = input$bw)
      }
    }
    grid.arrange(grobs = plots, ncol = 3, nrow = 4)
  })
}

shinyApp(ui, server)

# 1 Density plots r

library(dplyr)
library(ggplot2)
library(plotly)

#Ggplot2: histogram, density and violin plot

p1<-ggplot(mtcars, aes(mpg, fill=factor(am)))+geom_density(alpha=0.2)
p1

p2<-ggplot(mtcars, aes(mpg, fill=factor(am)))+geom_histogram(bins = 5)
p2

p3<-ggplot(mtcars, aes( y=mpg, x=factor(am)))+geom_violin(fill="orange")
p3

#Plotly: histogram, density and violin plot

p1<-plot_ly(mtcars, x=~mpg, color=~factor(am), alpha=0.6)%>%
  add_histogram()%>%
  layout(barmode = "overlay")

p1


#easiest from ggplot2:
p2<-ggplot(mtcars, aes(mpg, fill=factor(am)))+geom_density(alpha=0.6)
ggplotly(p2)

# Violin plot

p3<-plot_ly(mtcars, x=~factor(am), y=~mpg, split=~factor(am),
            type="violin", box=list(visible=T))
p3


# 2 splom r

library(dplyr)
library(ggplot2)
library(plotly)

#plotly

mtcars %>% plot_ly()%>%
  add_trace(type='splom',     dimensions = list(
    list(label='MPG', values=~mpg),
    list(label='Displacement', values=~disp),
    list(label='#Horse Power', values=~hp),
    list(label='weight', values=~wt)),
    marker = list(
      color = as.integer(mtcars$cyl),
      size = ~qsec
      
    )
    
  )

#ggplot2

library(GGally)
ggpairs(mtcars, columns=c(1,3,4,6),
        mapping =aes(color=factor(cyl), size=qsec))






# 3 surface contour R

library(dplyr)
library(ggplot2)
library(MASS)

library(plotly)

mtcars %>%plot_ly(x=~mpg, y=~disp, z=~wt, type="scatter3d")
mtcars %>%plot_ly(x=~mpg, y=~disp, z=~wt, type="contour")

library(akima)
attach(mtcars)
s=interp(mpg,disp,wt, duplicate = "mean")
detach(mtcars)

plot_ly(x=~s$x, y=~s$y, z=~s$z, type="surface")







# 4 


library(plotly)

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')



# WORLD MAP
g <- list(
  projection = list(type = 'Mercator')
)

p <- plot_geo(df) %>%
  add_trace(
    z = ~GDP..BILLIONS., color = ~GDP..BILLIONS., colors = 'Blues',
    text = ~COUNTRY, locations = ~CODE
  ) %>%
  layout(
    geo = g
  )

p

####Other country maps

rds<-readRDS("gadm36_GBR_2_sf.rds")
df<-read.csv("GBcities.csv")

rownames(df)=df$name
rds$Price=df[rds$NAME_2, "Price"]
#Data for some regions absent, setting to 0
rds$Price[is.na(rds$Price)]=0

#plotly
p<-plot_ly()%>%add_sf(data=rds, split=~NAME_2, color=~Price, showlegend=F, alpha=1)
p
#ggplot2
p<-ggplot(rds) +  geom_sf(aes(fill = Price))
ggplotly(p)

#mapbox

p<-plot_mapbox(rds, split=~NAME_2, color=~Price, showlegend=F, alpha=0.5)
p


















library(plotly)
library(dplyr)
# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  scope = 'north america'
)

p <- plot_geo(locationmode = 'USA-states', color = I("red")) %>%
  add_markers(
    data = air, x = ~long, y = ~lat, text = ~airport,
    size = ~cnt, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(flights, id),
    x = ~start_lon, xend = ~end_lon,
    y = ~start_lat, yend = ~end_lat,
    alpha = 0.3, size = I(1) , hoverinfo = "none"
  ) %>%
  layout(
    geo = geo
  )

p

















library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')


g <- list(
  scope = 'usa'
)

p <- plot_geo(df, lat = ~lat, lon = ~long) %>%
  add_markers(
    text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
    color = ~cnt, symbol = I("diamond"), hoverinfo = "text", 
    colors=colorRamp(c("lightblue","darkblue"))
  )  %>%
  layout(geo = g)
p


#size adjustment

p <- plot_geo(df, lat = ~lat, lon = ~long) %>%
  add_markers(
    text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
    size = ~cnt, symbol = I("diamond"), hoverinfo = "text", 
    colors=colorRamp(c("lightblue","darkblue"))
  )  %>%
  layout(geo = g)
p

## Other countries than USA- ggplot2

##Example -UK

rds<-readRDS("gadm36_GBR_0_sf.rds")
df<-read.csv("GBcities.csv")
p<-ggplot()+geom_sf(data=rds)+
  geom_point(data=df, mapping = aes(longitude, latitude, color=Price))


p
##Use google chrome- does not work in explorer.

ggplotly(p)


##Now with mapbox

p <- df %>%
  plot_mapbox(lat = ~latitude, lon = ~longitude,
              size = ~Price, 
              mode = 'scattermapbox')
p
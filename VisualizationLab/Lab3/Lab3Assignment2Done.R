library(ggplot2)
library(plotly)
library(dplyr)
library(MASS)
library(akima)
## Assignment 1

mosq<-read.csv("aegypti_albopictus.csv",header= TRUE, sep = ",")
head(mosq)


#Q1

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoib2JodXRhcmEiLCJhIjoiY2ptYm9yczN1MDcwMzNwbnlqMTY0eDl2bCJ9.IIY_aDN7MAQ8U_sok4xDww')
p10 <- mosq %>% filter(YEAR=='2004') %>% plot_mapbox(lat = ~Y, lon = ~X,
                                                     color = ~VECTOR, mode = 'scattermapbox',hoverinfo='name')

p11 <- mosq %>% filter(YEAR=='2013') %>% plot_mapbox(lat = ~Y, lon = ~X,
                                                     color = ~VECTOR, mode = 'scattermapbox',hoverinfo='name')



#Q2

mosqcountry <- mosq %>% group_by(COUNTRY,COUNTRY_ID) %>%  summarise(Z=n())
mosqcountry$hover<-with(mosqcountry, paste(mosqcountry$COUNTRY, '<br>', "Number of mosquito's are ", Z))
head(mosqcountry)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  projection = list(type = 'equirectangular')
)
tored <- c("#FFFF00","#FF0000")

p2 <- plot_geo(mosqcountry) %>%
  add_trace(
    z = ~Z,color = ~Z, colors = tored,
    text = ~paste(hover), locations = ~COUNTRY_ID
  ) %>%
  colorbar(title = "Number of Mosquitos") %>%
  layout(
    title = 'Number of mosquitos<br>(Hover for the number of mosquitos)',
    geo = g
  )
p2
#Q3a
# specify some map projection/options
g1 <- list(
  projection = list(type = 'azimuthal equidistant')
)
mosqcountry$logz=log(mosqcountry$Z)

p3a <- plot_geo(mosqcountry) %>%
  add_trace(
    z = ~Z,color = ~logz, colors = tored,
    text = ~paste(hover), locations = ~COUNTRY_ID
  ) %>%
  colorbar(title = "Number of Mosquitos") %>%
  layout(
    title = 'Number of mosquitos<br>(Hover for the number of mosquitos)',
    geo = g1
  )
p3a
#Q3b
g2 <- list(
  projection = list(type = 'conic equal area')
)

p3b <- plot_geo(mosqcountry) %>%
  add_trace(
    z = ~Z,color = ~logz, colors = tored,
    text = ~paste(hover), locations = ~COUNTRY_ID
  ) %>%
  colorbar(title = "Number of Mosquitos") %>%
  layout(
    title = 'Number of mosquitos<br>(Hover for the number of mosquitos)',
    geo = g2
  )
p3b


#Q4
mosqbrazil <- filter(mosq,COUNTRY_ID=="BRA",mosq$YEAR==2013)
mosqbrazil$X1<-cut_interval(as.numeric(mosqbrazil$X),n=100)
mosqbrazil$Y1<-cut_interval(as.numeric(mosqbrazil$Y),n=100)

head(mosqbrazil)
mosqbragrouped<- mosqbrazil %>%
  group_by(X1,Y1) %>% summarise("xlong"=mean(X),"ylat"=mean(Y),"totalmosq"=n())
head(mosqbragrouped)
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoib2JodXRhcmEiLCJhIjoiY2ptYm9yczN1MDcwMzNwbnlqMTY0eDl2bCJ9.IIY_aDN7MAQ8U_sok4xDww')
p4 <- mosqbragrouped %>%  plot_mapbox(lat = ~ylat, lon = ~xlong, color = ~totalmosq,
                                      mode = 'scattermapbox',hoverinfo= ~totalmosq) %>%
  layout(title = 'Most infected by mosquitos',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))

p4




## Assignment 2
## Reading and preprocessing data with swedish household income.
##Data Preprocessing     Run only once
data2 = read.csv("SwedishHousehold.csv", skip = 2, col.names = c("Region", "Age", "Income"))
head(data2)
my_strip <- function(region) {
  region = strsplit(as.character(region), " ")[[1]][2]
}
data2$Region = lapply(data2$Region, my_strip)
data2$Region = as.character(data2$Region)
levels(data2$Age) = c("Young", "Adult", "Senior")
data2_rshaped = reshape(data2, idvar = "Region", timevar = "Age", direction = "wide", v.names = NULL)
names(data2_rshaped) = c("Region", "Youth","Adult","Senior")
head(data2_rshaped)
##End of preprocessing

#2 violen plot
##This plot is shows that Income is highly dependent on the age of a person.
##Old people have a really high salary range and it is quiet similar for adults but
##the salary range is very low for the youth.
plot_ly(data2, x=~factor(Age), y=~Income, type="violin", 
        split=~factor(Age), box=list(visible=T)) %>%
  layout(title ="Violin plot for Each age group")


#3 Surface plot
##Yes I think linear regression would be a suitable model for this dependence.
## The range for adults and seniors is quiet similar and it is lower for the youth.
##I think Linear regression would give us a good fit to the data.
attach(data2_rshaped)
s=interp(Senior,Youth,Adult, duplicate = "mean")
detach(data2_rshaped)

plot_ly(x=~s$x, y=~s$y, z=~s$z, type="surface")%>%
  layout(title ="Surface Plot",scene = list(
    xaxis = list(title = "Senior"),
    yaxis = list(title = "Youth"),
    zaxis = list(title = "Adult")
  ))

#4 
##The previos plots had no information about the dependence of income on region. 
##This plot provides good information of how income is also dependent on the region you are in.
##The income is usually higher in the southern part of sweden compared to the northern part.
##According to this plot, the highest paying jobs are in Stockholm.
##Data Preprocessing     Run only once
rds<-readRDS("gadm36_SWE_1_sf.rds")
rownames(data2_rshaped)=data2_rshaped$Region
data2_rshaped["Västra", 1] = "Västra Götaland"
data2_rshaped["Örebro", 1] = "Orebro"
rownames(data2_rshaped)=data2_rshaped$Region
rds$Youth=data2_rshaped[rds$NAME_1, "Youth"]
rds$Adult=data2_rshaped[rds$NAME_1, "Adult"]
rds$Senior=data2_rshaped[rds$NAME_1, "Senior"]
##End of preprocessing

p_youth<-plot_ly()%>%
  add_sf(data=rds, split=~NAME_1, color=~Youth, showlegend=F, alpha=1)%>%
  layout(title ="Choropleth map showing income for youth")


p_adults<-plot_ly()%>%
  add_sf(data=rds, split=~NAME_1, color=~Adult, showlegend=F, alpha=1)%>%
  layout(title ="Choropleth map showing income for adults")

p_youth
p_adults

# 5 
##This is the same Choropleth map we had in the previous question with the red marker showing Linkoping City.
linkoping = read.csv("Linkoping.csv")
p_youth_withLoc<-plot_ly()%>%add_sf(data=rds, split=~NAME_1, 
                                    color=~Youth, showlegend=F, alpha=1)%>%
  add_markers(data = linkoping,
              y = ~latitude, x = ~longitude, text = ~desc)%>%
  layout(title ="Choropleth map showing income for youth")
p_youth_withLoc

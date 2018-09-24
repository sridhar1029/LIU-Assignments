library(ggplot2)
library(plotly)
library(dplyr)
library(MASS)
library(akima)

data1 = read.csv("aegypti_albopictus.csv", header = TRUE, row.names = 2)
head(data1)


## Assignment 2
# 1 reading data 

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
names(data2_rshaped) = c("Region","Adult","Senior","Youth")
head(data2_rshaped)
##End of preprocessing

#g = ggplot(data2, aes(Income, fill = factor(Age))) + geom_density()
#ggplotly(g)

#2 violen plot
p <- ggplot(data2, aes(x=Age, y=Income)) + 
  geom_violin()
p

ggplot(data2_rshaped) + 
  geom_violin(aes("Youth", Youth)) + 
  geom_violin(aes("Adult", Adult)) +
  geom_violin(aes("Senior", Senior))

plot_ly(data2, x=~factor(Age), y=~Income, type="violin", split=~factor(Age), box=list(visible=T))


#3 Surface plot

data2_rshaped %>%plot_ly(x=~Senior, y=~Youth, z=~Adult, type="scatter3d")

attach(data2_rshaped)
s=interp(Senior,Youth,Adult, duplicate = "mean")
detach(data2_rshaped)

plot_ly(x=~s$x, y=~s$y, z=~s$z, type="surface")

#4 

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

#merged_sweden = merge(rds,data2_rshaped, "NAME_1")

p_youth<-plot_ly()%>%add_sf(data=rds, split=~NAME_1, color=~Youth, showlegend=F, alpha=1)

p_adults<-plot_ly()%>%add_sf(data=rds, split=~NAME_1, color=~Adult, showlegend=F, alpha=1)

p_youth
p_adults
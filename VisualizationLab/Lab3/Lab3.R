library(ggplot2)
library(plotly)

data1 = read.csv("aegypti_albopictus.csv", header = TRUE, row.names = 2)
head(data1)


## Assignment 2
# 1 reading data 
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

#2 violen plot
p <- ggplot(data2, aes(x=Age, y=Income)) + 
  geom_violin()
p


#3 Surface plot



#4 
rds<-readRDS("gadm36_SWE_1_sf.rds")
rownames(data2_rshaped)=data2_rshaped$Region
data2_rshaped["Västra", 1] = "Västra Götaland"
data2_rshaped["Örebro", 1] = "Orebro"
rownames(data2_rshaped)=data2_rshaped$Region
rds$Youth=data2_rshaped[rds$NAME_1, "Youth"]
rds$Adult=data2_rshaped[rds$NAME_1, "Adult"]
rds$Senior=data2_rshaped[rds$NAME_1, "Senior"]

#merged_sweden = merge(rds,data2_rshaped, "NAME_1")

p_youth<-plot_ly()%>%add_sf(data=rds, split=~NAME_1, color=~Youth, showlegend=F, alpha=1)

p_adults<-plot_ly()%>%add_sf(data=rds, split=~NAME_1, color=~Adult, showlegend=F, alpha=1)

p_youth
p_adults


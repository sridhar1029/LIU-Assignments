library(ggplot2)
library(plotly)
library(seriation)

#1 read data
# Reading data into variable p_e. I am setting the first column of city names as Row names for the data.
# This is the reason I am subtracting 1 from the required columns we were asked to select.
p_e = read.table("prices-and-earnings.txt", header=T, sep = "\t", row.names = 1)
requiredCols = c(2,5,6,7,9,10,16,17,18,19) - 1
p_e = (p_e[,requiredCols])
head((p_e[,]))

#2  It is hard to find any clusters in this heatmap. It is even hard to spot outliers.
#There are 72 citys in the dataset,but we cannot see all the 72 on the y axis labels due to lack of space. 
#All the points are being plotted but its not shown on y axis labels as there is no space 
#for all of them.
#If we hover through the plot we would be able to find the corresponding city name.
p_e.numeric = (scale(p_e))
plot_ly(x=colnames(p_e.numeric), y=rownames(p_e.numeric), 
        z=p_e.numeric, type="heatmap", colors =colorRamp(c("yellow", "red")))%>%
  layout(title = "Heatmap without reordering")

#3 
#Both the plots re almost similar, its just both have different ordering of rows and cols
#due to the different optimization techniques used. It could also have been because of 
#the different ways we used initially before reordering, euclidian distance and one minus
#correlation. But the clusters found in both the heatmaps are similar.
row_dis = dist(x = p_e.numeric, method = "euclidean", diag = T)
col_dis = dist(x = t(p_e.numeric), method = "euclidean", diag = T)
row_cor = 1 - cor(t(p_e.numeric))
col_cor = 1 - cor(p_e.numeric)
order1_euc = seriate(row_dis, "OLO")
order2_euc = seriate(col_dis, "OLO")
ord1_euc = get_order(order1_euc)
ord2_euc = get_order(order2_euc)
order1_cor = seriate(as.dist(row_cor), "OLO")
order2_cor = seriate(as.dist(col_cor), "OLO")
ord1_cor = get_order(order1_cor)
ord2_cor = get_order(order2_cor)

reordered_euc = p_e.numeric[rev(ord1_euc), ord2_euc]
reordered_cor = p_e.numeric[rev(ord1_cor), ord2_cor]

plot_ly(x=colnames(reordered_euc), y=rownames(reordered_euc), 
        z=reordered_euc, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% 
  layout(title = "Heatmap of reordered matrix using eucledian distance")

plot_ly(x=colnames(reordered_cor), y=rownames(reordered_cor), 
        z=reordered_cor, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% 
  layout(title = "Heatmap of reordered matrix using correlations")



#4
order1_tsp = seriate(row_dis, "TSP")
order2_tsp = seriate(col_dis, "TSP")
ord1_tsp = get_order(order1_tsp)
ord2_tsp = get_order(order2_tsp)

reordered_tsp = p_e.numeric[rev(ord1_tsp), ord2_tsp]


plot_ly(x=colnames(reordered_tsp), y=rownames(reordered_tsp), 
        z=reordered_tsp, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% 
  layout(title = "Heatmap of reordered matrix using TSP")
###criterion
criterion(dist(reordered_euc), method = c("Gradient_raw", "Path_length"))
criterion(dist(reordered_tsp), method = c("Gradient_raw", "Path_length"))
rbind("HC" = criterion(dist(reordered_euc), method = c("Gradient_raw", "Path_length")),
      "TSP" = criterion(dist(reordered_tsp), method = c("Gradient_raw", "Path_length")))

##left out


#5
colnames(p_e) = c("FoodCosts", "iPhone4S", "ClothingIndex", "Hours", "NetWage", "VacationDays", 
                          "BigMac", "Bread", "Rice", "GoodsServices")
p_e %>% plot_ly(type = 'parcoords', 
                            dimensions = list(
                              list(label = "Food Costs", values = ~FoodCosts),
                              list(label = "iPhone 4S", values = ~iPhone4S),
                              list(label = "Clothing Index", values = ~ClothingIndex),
                              list(label = "Hours Worked", values = ~Hours),
                              list(label = "Wage Net", values = ~NetWage),
                              list(label = "Vacation Days", values = ~VacationDays),
                              list(label = "Big Mac", values = ~BigMac),
                              list(label = "Bread", values = ~Bread),
                              list(label = "Rice", values = ~Rice),
                              list(label = "Goods Services", values = ~GoodsServices)))

library(GGally)
d=p_e
d$iPhone4S_cluster=ifelse(d$iPhone4S <= 60, 1, 0)
obj <- ggparcoord(d, scale="uniminmax", groupColumn = "iPhone4S_cluster")

ggplotly(obj)

d %>% plot_ly(type = 'parcoords',
                line = list(
                  color = ~iPhone4S_cluster,
                  colorscale = list(
                    c(0, 'green'),
                    c(1, 'red')
                  )
                ), 
                dimensions = list(
                  list(label = "Food Costs", values = ~FoodCosts),
                  list(label = "iPhone 4S", values = ~iPhone4S),
                  list(label = "Clothing Index", values = ~ClothingIndex),
                  list(label = "Hours Worked", values = ~Hours),
                  list(label = "Wage Net", values = ~NetWage),
                  list(label = "Vacation Days", values = ~VacationDays),
                  list(label = "Big Mac", values = ~BigMac),
                  list(label = "Bread", values = ~Bread),
                  list(label = "Rice", values = ~Rice),
                  list(label = "Goods Services", values = ~GoodsServices)))


#6

reordered_euc <- as.data.frame(reordered_euc)
reordered_euc$City = row.names(reordered_euc)

reuc_transformed <- reordered_euc%>%tidyr::gather(variable, value, -City, factor_key=T)%>%arrange(City)

radar_plot <- reuc_transformed %>% ggplot(aes(x=variable, y=value, group=City)) + geom_polygon(col = 'red', fill="blue", alpha = 0.5) + coord_polar() + theme_bw() + facet_wrap(~ City) + theme(axis.text.x = element_text(size = 5))

ggsave("radar_plot.png", width = 40, height = 60, units = "cm")

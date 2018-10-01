library(ggplot2)
library(plotly)
library(seriation)

#1 read data
p_e = read.table("prices-and-earnings.txt", header=T, sep = "\t", row.names = 1)
requiredCols = c(2,5,6,7,9,10,16,17,18,19) - 1
p_e = (p_e[,requiredCols])
head((p_e[,]))

#2
p_e.numeric = (scale(p_e))
plot_ly(x=colnames(p_e.numeric), y=rownames(p_e.numeric), 
        z=p_e.numeric, type="heatmap", colors =colorRamp(c("yellow", "red")))%>%
  layout(title = "Heatmap without reordering")

#3
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
  layout(title = "Heatmap of reordered matrix using correlations")
###criterion
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

d=p_e
d$iPhone4S_cluster=ifelse(d$iPhone4S <= 60, 1, 0)
obj <- ggparcoord(d, scale="uniminmax", groupColumn = "iPhone4S_cluster")

ggplotly(obj)

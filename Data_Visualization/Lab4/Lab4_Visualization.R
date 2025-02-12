library(data.table)
library(dplyr)
library(scales)
library(plotly)
library(ggplot2)
library(seriation)
library(GGally)

### 1. Reading input and filter column by index

price_data <- read.delim("prices-and-earnings.txt")
price_data <- price_data[,c(1,2,5,6,7,9,10,16,17,18,19)]



### 2. Heatmap of data without reorder
price_data_scale <- scale(price_data[,-1])

plot_ly(x=colnames(price_data_scale), y=rownames(price_data_scale), 
        z=price_data_scale, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% layout(title = "Heatmap of price dataset")




### 3. Distance matrix computation and ordering by Hierarchical Clustering
price_row_dist <- dist(x=price_data_scale, method = "euclidean", diag = TRUE)
price_col_dist <- dist(x=t(price_data_scale), method = "euclidean", diag = TRUE)
price_row_cor <- cor(x=t(price_data_scale))
price_col_cor <- cor(x=price_data_scale)
price_row_cor <- 1- price_row_cor
price_col_cor <- 1- price_col_cor

order1 <- seriate(price_row_dist, "OLO")
order2 <- seriate(price_col_dist, "OLO")
ord1 <- get_order(order1)
ord2 <- get_order(order2)

order1_cor <- seriate(as.dist(price_row_cor), "OLO")
order2_cor <- seriate(as.dist(price_col_cor), "OLO")
ord1_cor <- get_order(order1_cor)
ord2_cor <- get_order(order2_cor)

reordmatr <- price_data_scale[rev(ord1),ord2]
reordmatr_cor <- price_data_scale[rev(ord1_cor),ord2_cor]

plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
        z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% layout(title = "Heatmap of price dataset ordered using Optimal leaf ordering")

plot_ly(x=colnames(reordmatr_cor), y=rownames(reordmatr_cor), 
        z=reordmatr_cor, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% layout(title = "Heatmap of correlations of price dataset")



### 4. Distance matrix computation and ordering by Traveling Sales Man
order1_tsp <- seriate(price_row_dist, "TSP")
order2_tsp <- seriate(price_col_dist, "TSP")
ord1_tsp <- get_order(order1_tsp)
ord2_tsp <- get_order(order2_tsp)
reordmatr <- price_data_scale[rev(ord1_tsp),ord2_tsp]

plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
        z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red"))) %>% layout(title = "Heatmap of price dataset ordered using Traveling Sales Man")

## Comparing the optimization
criterion(dist(price_data_scale[rev(ord1_tsp)], method = "euclidean"), method = c("Gradient_raw", "Path_length"))
criterion(dist(price_data_scale[rev(ord1)], method = "euclidean"), method = c("Gradient_raw", "Path_length"))
criterion(dist(price_data_scale))


### 5. Parallel Coordinate Plots

setnames(price_data, old = c("Food.Costs...", "iPhone.4S.hr.", "Clothing.Index", "Hours.Worked", 
                             "Wage.Net", "Vacation.Days", "Big.Mac.min.", "Bread.kg.in.min.", 
                             "Rice.kg.in.min.", "Goods.and.Services..."), 
         new=c("FoodCosts", "iPhone4S", "ClothingIndex", "Hours", "NetWage", "VacationDays", 
               "BigMac", "Bread", "Rice", "GoodsServices"))

price_data[,-1] %>% plot_ly(type = 'parcoords', 
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

#coloring lines
data=price_data[,-1]
data$BigMac_cluster=ifelse(data$BigMac <= 21, 1, 0)
obj <- ggparcoord(data, scale="uniminmax", groupColumn = "BigMac_cluster")

ggplotly(obj)



### 6. Radar Chart Plots
reordmatr <- as.data.frame(reordmatr)
reordmatr$City = price_data$City

reordmatr_transformed <- reordmatr%>%tidyr::gather(variable, value, -City, factor_key=T)%>%arrange(City)

radar_plot <- reordmatr_transformed %>% ggplot(aes(x=variable, y=value, group=City)) + geom_polygon(fill="blue") + coord_polar() + theme_bw() + facet_wrap(~ City) + theme(axis.text.x = element_text(size = 5))

ggsave("radar_plot.png", width = 40, height = 60, units = "cm")






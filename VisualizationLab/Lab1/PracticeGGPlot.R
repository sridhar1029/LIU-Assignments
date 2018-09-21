library(ggplot2)

data("iris")
head(iris)
summary(iris)
plot(iris)
plot(iris$Petal.Length, iris$Sepal.Width)


#ggplot   aes-aesthetic mapping      point_geom
#we use aesthetic mapping to control the way our variables are represented
#across our plots
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) + 
  geom_point()


ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species)) + 
  geom_point()


ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, size = Petal.Width)) + 
  geom_point()



ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, size = Petal.Width,
                 shape = Species)) + 
  geom_point()


ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, col = Species, size = Petal.Width,
                 shape = Species, alpha = Sepal.Length)) + 
  geom_point()


#Bar plots        Histograms

ggplot(iris, aes(Species, Sepal.Length, fill = Species)) + 
  geom_bar(stat = "summary", fun.y = "mean") 

ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill = "#bb1176", col = "black")


ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill = "#bb1176", col = "black") + 
  geom_point()


myPlot = ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_bar(stat = "summary", fun.y = "mean", fill = "#bb1176", col = "black") + 
  geom_point(position = position_jitter(0.2), size = 3, shape = 22, fill = "blue")

myPlot

myPlot + theme(panel.grid = element_blank(), 
               panel.background = element_rect(fill="white"),
               axis.line.y = element_line(colour = "black", size = 0.2),
               axis.line.x = element_line(colour = "black", size = 0.2))

myPlot = myPlot + theme_dark() + theme(panel.grid = element_blank())


#NOTE : We are layering things on top of thher things. So the order matters

#Box ploys
ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot(fill = "green", col = "black", notch = TRUE) + 
  geom_point()


#finishing touches
myPlot + labs(x = "", y = "Sepal Length (mm)") + 
  ggtitle("Sepal Length By IRIS species") + 
  theme(plot.title = element_text(hjust = 0.5))


#Saving our plot
ggsave("Plot1.pdf", width = 8, height = 5, units = "in")
ggsave("Plot1.png", width = 8, height = 5, units = "in")



#Factorial Data Set
data("ToothGrowth")
head(ToothGrowth)
summary(ToothGrowth)

ggplot(ToothGrowth, aes(supp, len, fill = as.factor(dose))) + 
  geom_bar(stat = "summary", fun.y = "median", 
           color = "black", position = "dodge") + 
  geom_point(position = position_dodge(0.9))


#line plot with linear regression
ggplot(ToothGrowth, aes(as.factor(dose), len, group = supp, col = supp)) + 
  geom_line(stat = "summary", fun.y = "mean") + 
  geom_smooth(method = "lm")

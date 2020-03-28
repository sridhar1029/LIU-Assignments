library(RMaCzek)
## code from RMaCzek's manual
## https://cran.r-project.org/web/packages/RMaCzek/
# Set data ####
x<-czek_matrix(mtcars)
# Standard plot ############
plot(x)
plot.czek_matrix(x)

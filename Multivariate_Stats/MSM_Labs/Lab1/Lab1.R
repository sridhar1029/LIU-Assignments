# QUESTION 2
dataset = T1_9

# a)
cov_matrix <- cov(dataset[, 2:8])
cor_matrix <- cor(dataset[, 2:8])

# b)
pairs(dataset[, 2:8])
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(dataset[, 2:8], lower.panel = panel.cor)

# c)
library(corrplot)
library(ellipse)
corrplot.mixed(cov_matrix, is.corr = FALSE, upper = "number", lower = "circle")
corrplot.mixed(cor_matrix, upper = "number", lower = "circle")
plotcorr(cov_matrix, type="lower", diag=FALSE, main="Bivariate correlations")

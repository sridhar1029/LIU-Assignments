d = read.table("SENIC.txt", header = F, sep = "")

#Q2

quantiles = function(r){
  q = quantile(r)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  v1 = q3 + 1.5*(q3-q1)
  v2 = q1 - 1.5*(q3-q1)
  #print(c(v1, v2, q1, q3))
  u = union(which((r>v1)), which((r<v2)))
  #print(r[u])
  return(u)
}
quantiles(d[,4])


head(d)

#Q3

library(ggplot2)
library(gridExtra)
library(grid)
r = 4
q = quantiles(d[,r])

plotQ3 = ggplot(d, aes(V4)) + geom_density() +
  geom_jitter(data = data.frame(x = d[,4][q], y = 0), aes(x, y), height = 0, shape = 5)

ggplot(d, aes_string(paste("V", r, sep = ""))) + geom_density() +
  geom_jitter(data = data.frame(x = d[,r][q], y = 0), aes(x, y), height = 0, shape = 5)


#ggplot(d, aes(V4, V1, shape = ifelse(d[,r] %in% q, "diamond", "square"), col = ifelse(d[,r] %in% q,"A", "B")))+
#  geom_point() +
#  geom_vline(xintercept = c(1.45, 7.45)) 
hist(d[,4])


#Q4
plots = list()
for(i in 2:12){
  outliers = quantiles(d[,i])
  colSel = paste("V", i, sep = "")
  plotName = paste("P", i, sep = "")
  if(identical(outliers, integer(0))){
    next
  }
  df = data.frame(x = d[,i][outliers], y = 0)
  plots[[plotName]] = ggplot(d, aes_string(colSel)) + geom_density() + 
    geom_jitter(data = df, aes(x, y), height = 0, shape = 05)
}
#pdf("all.pdf")
#invisible(lapply(plots, print))
#dev.off()

#invisible(mapply(ggsave, file=paste0("plot-", names(plots), ".png"), plot=plots))

grid.arrange(plots, nrow = 4, ncol = 3)
allPlots = do.call("grid.arrange", c(plots, ncol=3))
allPlots
arrangeGrob(plots, nrow = 3)

#ggsave("All.png", width = 9, height = 9, units = "in")



#Q5
a = ggplot(d, aes(V4, V11, col = V7)) + geom_point()
a


#Q6
library(plotly)
ggplotly(plotQ3)
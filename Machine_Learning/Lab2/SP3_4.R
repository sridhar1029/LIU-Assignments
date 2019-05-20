#SP3
library(MASS)
library(ggplot2)
##1
aus_crabs = read.csv("australian-crabs.csv", header = TRUE, sep = ',')
head(aus_crabs)
ggplot(aus_crabs, aes(CL, RW, col=sex)) + geom_point()
df = aus_crabs[,c('CL', 'RW', 'sex')]
y = aus_crabs$sex
group1_index = which( y == 'Male' )
group2_index = which( y == 'Female' )
#SB
mu = as.matrix(colMeans(df[,c('CL', 'RW')]))
mu1 = as.matrix(colMeans(df[group1_index,c('CL', 'RW')]))
mu2 = as.matrix(colMeans(df[group2_index,c('CL', 'RW')]))
temp = mu1-mu
u1 = temp%*%t(temp)*length(group1_index)
temp = mu2-mu
u2 = temp%*%t(temp)*length(group2_index)
sb = u1 + u2
#SW
temp1 = df[group1_index,]
temp1$CL = temp1$CL - mu[1]
temp1$RW = temp1$RW - mu[2]
temp2 = df[group2_index,]
temp2$CL = temp2$CL - mu[1]
temp2$RW = temp2$RW - mu[2]
temp = rbind(temp1, temp2)
sw = matrix(0, nrow = 2, ncol = 2)
for(i in 1:nrow(temp)){
  o = as.matrix(temp[i,c('CL', 'RW')])
  sw = sw + (t(o)%*%o)
}
s = ginv(sw)%*%sb
v1 = s[,1]
v2 = s[,2]
x = seq(1,10,0.1)
y1 = v1[1] + v1[2]*x
y2 = v2[1] + v2[2]*x
dd = data.frame(x, y1, y2)

plot(df$CL, df$RW, xlim = c(-5,50), ylim = c(-10,20))
points(x, y1)
points(x, y2)
#projections
p1 = as.matrix(df[,c('CL', 'RW')])
p2 = as.matrix(v1)
p = p1%*%p2
plot(p)
df_proj = data.frame(id = row.names(df),x = p, y = df$sex)

ggplot(df_proj, aes(x, id, col=y)) + geom_point()

ggplot(df_proj, aes(x, y, col=y)) + geom_point()


##SP4
#Q1
x1 <- 1/3
x2 <- 1/4
if(x1-x2 == 1/12){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
}

"
The way 1/3 is represented in r is makes the difference in this case. Since the representation 
of 1/3 is different, the subtraction (1/3 - 1/4) results in a different answer than 1/12.
The difference occurs in the 17th decimal place. The difference is very small but this is what
results in the inequality in the first case.
"

x1 <- 1
x2 <- 1/2
if(x1-x2 == 1/2){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
} 

"
1/2 is not such a complex fraction so the representation is easy for it and results in 
x2 being equal to (0.5). This is the reason  the equality is true in this case.
1 - 1/2 is 0.5 and 1/2 is also 0.5, resulting in the equality.


The way we can improve this is comparing only a few of the decimal places, like the first
10 decimal places of a number.
"

x1 <- 1/3
x2 <- 1/4
a = formatC(x1-x2, digits = 10, format = 'f')
b = formatC(1/12, digits = 10, format = 'f')
if(a == b){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
} 


#Q2
f =function(x){
  return(x)
}
derivative = function(f, x){
  eps = 10^-15
  d = (f(x+eps) - f(x))/(eps)
  return(d)
}
derivative(f, 1)
derivative(f, 100000)


#Q3
myVar = function(x){
  mu = (sum(x)^2)/length(x)
  s = sum(x^2)
  vr = (s - mu)/(length(x)-1)
  return(vr)
}
x = rnorm(10000, 10^8, 1)
Yi = matrix(0, nrow = 1, ncol = 10000)
for(i in 2:10000){
  Xi = x[1:i]
  Yi[i] = myVar(Xi) - var(Xi)
}
plot(1:10000, Yi)


myVar2 = function(x){
  mu = (sum(x)^2)/length(x)
  s = (x^2) - mu
  vr = (sum(s))/(length(x)-1)
  return(vr)
}
x = rnorm(10000, 10^8, 1)
Yi = matrix(0, nrow = 1, ncol = 10000)
for(i in 2:10000){
  Xi = x[1:i]
  Yi[i] = myVar2(Xi) - var(Xi)
}
plot(1:10000, Yi)


#Q4
data = read.csv("tecator.csv", row.names = 1)

y_data = as.matrix(data$Protein)
x_data = as.matrix(data[,- c(102)])

A = t(x_data)%*%x_data
b = t(x_data)%*%y_data

solve(A, b)

kappa(A)


#scaling data
x_data_scaled = scale(x_data)

A = t(x_data_scaled)%*%x_data_scaled
b = t(x_data_scaled)%*%y_data

solve(A, b)

kappa(A)

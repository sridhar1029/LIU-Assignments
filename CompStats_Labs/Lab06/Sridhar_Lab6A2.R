library(ggplot2)

physical_data = read.csv("physical1.csv")
head(physical_data)

ggplot(physical_data) + 
  geom_line(aes(X, Y, color='Y')) + 
  geom_line(aes(X, Z, color='Z')) + 
  ggtitle("Plot of Y and Z vs X")


##EM
EM.Exp<-function(data, eps, kmax, lambda_0){
  X <- data$X; Y <- data$Y; Z <- data$Z
  Xobs <- X[!is.na(Z)]
  Zobs <- Z[!is.na(Z)]
  Zmiss <- Z[is.na(Z)]
  n <- length(X)
  m <- length(Zmiss)
  k <<- 0
  llvalprev <- 0
  llvalcurr <- lambda_0
  cat("Initial lambda value : ", llvalcurr)
  
  while ((abs(llvalprev-llvalcurr)>eps) && (k<(kmax+1))){
    llvalprev<-llvalcurr
    llvalprev <- llvalcurr
    llvalcurr <- (sum(X*Y)+sum(Xobs*Zobs)/2+m*llvalprev)/(2*n)
    k <<- k+1
  }
  return(c(llvalprev,llvalcurr,k))
}
lmb = EM.Exp(physical_data,0.001,100,100)
cat("The final converged lambda value : ", lmb[1])
cat("Number of steps taken to converge : ", lmb[3])
lambda <- lmb[2]
new_data <- physical_data
new_data$E_Y <- lambda/physical_data$X
new_data$E_Z <- 2*lambda/physical_data$X
ggplot(data=new_data) +
  geom_line(aes(x = X, y = Y, colour = "Y")) +
  geom_line(aes(x = X, y = Z, colour = "Z")) +
  geom_line(aes(x = X, y = E_Y, colour = "E_Y")) +
  geom_line(aes(x = X, y = E_Z, colour = "E_Z")) +
  ggtitle("Plot of Y,Z and their expected value vs. X")

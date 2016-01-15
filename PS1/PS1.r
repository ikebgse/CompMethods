#setwd("C:/Users/MF/Documents/BGSE/TERM 2/computational methods/problem sets/PS1")

##########################
####### Exercise 1 #######
##########################

# n = number of observations 

genData <- function(n){
  x <- seq(0, n, by = 1)
  y1 <- sin(5*pi*x)+ 0.25*rnorm(length(x))
  y2 <- y1*0.5

  pdf("SampleGraph.pdf",width=7,height=5)
  plot(x, y1, type = "l", col="blue", ylab="y")
  lines(x, y2, type = "l", col="red")
  dev.off()
  
  df <- data.frame( x = x, "signal.1" = y1, "signal.2" = y2)

  write.csv(df, file = "dataset.csv", row.names = FALSE)
  
  return(df)  
}


##########################
####### Exercise 2 #######
##########################

library(mvtnorm)
library(ggplot2)

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}


loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

loanDf <- loanData(noApproved=50, noDenied=50, c(4, 150), c(10, 100), 
                   c(1,20), c(2,30), -0.1, 0.6, 1221)


# adding new data

undetectedSolvency<-seq(80,170,length.out = 50)
undetectedPI<-seq(5,12,length.out = 50)+rnorm(50,0,1)

new.data<-cbind(undetectedPI, undetectedSolvency, rep('Undecided',50), rep(0,50))
colnames(new.data)<-c("PIratio", "solvency", "deny", "target")

data<-rbind(loanDf, new.data)
data[,c(1,2,4)]<-apply(data[,c(1,2,4)],2,as.numeric)


data$target1<-c( rep(1,50), rep(0,100))
data$target2<-c( rep(0,100), rep(1,50))



# creating bounds

datafit <- lm(as.numeric(target) ~ solvency + PIratio + 1, data=data)

weights <- coef(datafit)[c("solvency", "PIratio")]
bias <- coef(datafit)[1]

intercept <- (-bias + 0.5)/weights["PIratio"]
slope <- -(weights["solvency"]/weights["PIratio"])
bound<-c(intercept, slope)

datafit1 <- lm(as.numeric(target1) ~ solvency + PIratio + 1, data=data)
weights <- coef(datafit1)[c("solvency", "PIratio")]
bias <- coef(datafit1)[1]

intercept <- (-bias + 0.5)/weights["PIratio"]
slope <- -(weights["solvency"]/weights["PIratio"])
bound1<-c(intercept, slope)

# saving and plotting the data

final.data <- data[,1:3]

final.data$approved <- predict.lm(datafit1)
final.data$approved[final.data$approved >= 0.5] <- 1
final.data$approved[final.data$approved < 0.5] <- 0

final.data$denied <- predict.lm(datafit)
final.data$denied[final.data$denied >= 0.5] <- 1
final.data$denied[final.data$denied < 0.5] <- 0

final.data$undecided <- 0
final.data$undecided[final.data$approved == 0 & final.data$denied ==0] <- 1

# exporting csv

write.csv(final.data, file = "predictions.csv", row.names = FALSE)

# exporting pdf

pdf('discFunction3C.pdf')
print(ggplot(data = data, aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
        geom_point(size=3) +
        xlab("Solvency") +
        ylab("Weight") +
        theme_bw() +
        geom_abline(intercept = bound[1], slope = bound[2])+
        geom_abline(intercept = bound1[1], slope = bound1[2]))

dev.off()
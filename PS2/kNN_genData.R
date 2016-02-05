#setwd("C:/Users/MF/Documents/BGSE/TERM 2/computational methods/problem sets/PS4")

#install.packages("assertthat")
#install.packages("flexclust")
library(assertthat)
library(flexclust)



kNN <- function(features, label, memory = NULL, k = 1, p = 2, type="train") {
  
  #creating a normalizing function
  normalize <- function(x) {return ((x-min(x))/(max(x)-min(x)))}
  
  # verifying that the inputs are in the correct format
  not_empty(features)
  not_empty(label)
  assert_that(k %in% 1:nrow(features))
  assert_that(p %in% c(1, 2, Inf))
  assert_that(type %in% c("train", "predict"))
  
  if (type == "train") {
    assert_that(nrow(features) == length(label))
  }
  
  else if (type == "predict") {
    assert_that(not_empty(memory) & ncol(memory) == ncol(features))
  }
  
  # defining the features matrix
  features <- as.data.frame(lapply(features, normalize))
  
  # calculating the distance between the data points
  distMethod <- ifelse(p==1, 'manhattan', ifelse(p==2,'euclidean','maximum'))
  
  if (type == "train") {
    noObs <- nrow(features)
    distMatrix <- as.matrix(dist(features, method = distMethod))
  }
  
  else if (type == "predict") {
    noObs <- nrow(memory)
    memory <- as.data.frame(lapply(memory, normalize))
    distMatrix <- as.matrix(dist2(memory,features,method = distMethod))
  }
  
  #sorting distances and choosing k elements and checking for the most occurring label
  neighbors <- apply(distMatrix, 1, order)
  predLabels <- rep(NA, noObs)
  prob <- rep(NA, noObs)
  
  for (obs in 1:noObs) {
    predLabels[obs] <- as.numeric(names(tail(sort(table(label[neighbors[1:k, obs]])),1)))
    prob[obs] <- round(tail(sort(table(label[neighbors[1:k, obs]])),1)/k,4)
  }
  
  #evaluating the accuracy of the model
  if (type == "train") {
    errorCount <- table(predLabels, label)
    accuracy <- mean(predLabels == label)
  }
  
  else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  #final results
  return(list(predLabels = predLabels, 
              prob = prob,
              accuracy = accuracy,
              errorCount = errorCount))
}



genSpirals <- function(N = 200,
                       degrees = 570,
                       location = 90,
                       blend = 0.2,
                       saveData = TRUE, 
                       savePlot = TRUE) {
  
  # Generate two-spiral data 
  # idea of the problematic dataset: http://www.benmargolis.com/compsci/ai/two_spirals_problem.htm
  # N - number of observations
  # degrees - length of the spiral 
  # location - how far away from the origin
  # blend<-blending together
  
  #necessary packages
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  
  # define some variables
  degrees2rad <- (2*pi)/360 #convert degrees to radiant
  location <- location*degrees2rad #how far away from 00 the spiral starts
  
  N1 <- floor(N/2)
  N2 <- N-N1
  
  #spiral 1 
  #we indicate it by 0 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d1 <- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
  
  #the second spiral we indicate by 1 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d2 <-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
  
  #combine the data 
  data <- data.frame(rbind(d1, d2))
  names(data) <- c("x1", "x2", "y")
  
  #create CSV
  if (saveData) {
    write.csv(data, "dataset.csv", row.names = FALSE)
  }
  
  #create pdf plot
  if (savePlot) {
    cairo_pdf("plot.pdf")
    print(
      ggplot(data = data, 
             aes(x = x1, y = x2, colour=y)) + 
        scale_colour_continuous(guide = FALSE) +
        geom_point() +
        ggtitle("Spirals") +
        xlab("x1") +
        ylab("x2") +
        theme_bw()
    )
    dev.off()
  }
  
  return(data)
}

#generating and labeling the data
dataset <- genSpirals()
features <- dataset[,c(1,2)]
labels <- dataset$y

finaldata <- kNN(features,labels, k = 3, p = 2, type = "train")

predictions <- data.frame(cbind(dataset, 
                                predLabels = finaldata$predLabels, 
                                prob= finaldata$prob))

colnames(predictions) <- c("1stDimension","2ndDimension","TrueLabel","PredictedLabel","Prob")

predictions$ClassLabel <- factor(predictions$ClassLabel)

write.csv(predictions, file="predictions.csv", row.names = FALSE)

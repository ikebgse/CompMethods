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

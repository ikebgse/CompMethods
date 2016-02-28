library(assertthat)

#Defining cost functions
ME <- function(prob) {
  MissError <- 1 - apply(prob,1,max)
  return(MissError) 
}

Gini <- function(prob) {
  Gini <- rowSums(prob*(1-prob))
  return(Gini)
}

Entropy <- function(prob) {
  prob[prob == 0] <- 10^-10
  CrossEntropy <- - rowSums(prob*log(prob))
  return(CrossEntropy)
}

#Defining threshold function
findThreshold <- function(x, y, costFnc) {

  minError <- Inf
  bestThreshold <- NA
  labels <- NA
  y <- as.factor(y)
  
  if(length(unique(x)) > 1){  
    
    xroll <- rollmean(sort(unique(x)),2)
    noPoints <- length(x)
    
    Splitinfo <- lapply(xroll,function(z) {     
      potThres <- z
      predictedClasses <- rep(NA, 2)
      predictedClasses[1] <- as.numeric(names(tail(sort(table(y[x <= potThres])),1)))
      predictedClasses[2] <- as.numeric(names(tail(sort(table(y[x > potThres])),1)))
      
      prob <- matrix(NA,
                     nrow = 2,
                     ncol = length(unique(y)))
      
      prob[1,] <- table(y[x <= potThres])/length(y[x <= potThres])
      prob[2,] <- table(y[x > potThres])/length(y[x > potThres])
      
      #defining a cost function
      cost <- sum(costFnc(prob))
      
      return(list(error = cost, 
                  threshold = potThres, 
                  labels = predictedClasses))
    })
    
    Splitinfo <- data.frame(t(simplify2array(Splitinfo,higher = TRUE)))
    
    #evaluating the best threshold where the cost is minimized
    minError <- min(abs(unlist(Splitinfo[[1]])))
    ind <- which(Splitinfo[[1]] %in% c(minError,-minError))[1]
    bestThreshold <- Splitinfo[[ind,2]]
    
    #labels of the best split
    labels <- Splitinfo[[ind,3]]
  }
  
  #returning the error, threshold and labels in case the points are lined up along the same coordinate
  return(list(err = minError,
              thres = bestThreshold, 
              labels = labels))
}

cTree <- function(formula, data, depth, minPoints, costFnc = Entropy, type = "train", test.data = NULL){
  
  #checking the data structure and correctness of parameters
  not_empty(data);not_empty(formula);
  is.count(depth); is.count(minPoints);
  is.string(type); assert_that(type %in% c("train", "predict"));
  
  #defining the response variable
  m.frame <- model.frame(formula, data)
  Terms = attr(m.frame, "terms")
  Y <- model.response(m.frame)
  coln = attr(Terms, "term.labels")
  X <- subset(m.frame,select = coln)
  ncol = ncol(X)
  nrow = nrow(X)
  predLabels = rep(NA,nrow)
  prob = rep(NA,nrow)
  
  if(nrow(X) >= minPoints & depth > 0 & length(unique(Y)) > 1){
    
    results <- apply(X,2,function(x) findThreshold(x, Y,costFnc))
    
    Splitinfo <- data.frame(t(simplify2array(results,higher = TRUE)))

    #getting the minimum error and the best threshold
    minError <- min(unlist(Splitinfo[[1]]))
      
      col.ind <- which(Splitinfo[[1]]==minError)[1]
      bestThreshold <- Splitinfo[[col.ind,2]]
      
      #getting the final labels of the best split
      labels <- Splitinfo[[col.ind,3]]
      
      row.ind <- which(X[,col.ind] <= bestThreshold)
      Xleft <- m.frame[row.ind,]
      Xright <- m.frame[-row.ind,]
      
      #defining splits
      if(type == "predict"){
        
        test.row.ind <- which(test.data[,col.ind] <= bestThreshold)
        test.left <- data.frame(test.data[test.row.ind,])
        colnames(test.left) = colnames(X)
        test.right <- data.frame(test.data[-test.row.ind,])
        colnames(test.right) = colnames(X)
        
        left <- cTree(formula, data = Xleft , depth = depth -1, 
                      minPoints , costFnc , type , test.data = test.left )
        right <- cTree(formula, data = Xright , depth = depth -1, 
                       minPoints, costFnc , type , test.data = test.right )
      }
      
      else{
        left <- cTree(formula,
                      data = Xleft,
                      depth = depth -1,
                      minPoints,
                      costFnc,
                      type)
        
        right <- cTree(formula,
                       data = Xright,
                       depth = depth - 1,
                       minPoints,
                       costFnc,
                       type)
      }
      
      #fetching labels and associated probabilities
      predLabels[row.ind] <- left$predLabels
      prob[row.ind] <- left$prob
      predLabels[-row.ind] <- right$predLabels
      prob[-row.ind] <- right$prob
      
      if(type == "predict"){
        testLabels <- rep(NA,nrow(test.data))
        testLabels[test.row.ind] <- left$testLabels
        testLabels[-test.row.ind] <- right$testLabels

        return(list(predLabels = predLabels,
                    prob = prob,
                    testLabels = testLabels))
      }
      
      else{
            return(list(predLabels = predLabels,
                        prob = prob))
      }
  }
  
  assignedClass <- as.numeric(names(tail(sort(table(Y)),1)))
  prob <- tail(sort(table(Y)),1)/length(Y)
  
  if(type == "predict"){
    return(list(predLabels = assignedClass,
                prob = prob,
                testLabels  = assignedClass))
  }
  
  else{
    return(list(predLabels = assignedClass,
                prob = prob))
  }
}

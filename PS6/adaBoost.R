#making sure the labels are in the correct format
regFunction <- function(Y, rev = F, levels = NULL){
  
  if(rev == F){
    Y <- as.numeric(as.character(Y))
    levels <- sort(unique(Y))
    Y[Y %in% levels[1]] <- -1
    Y[Y %in% levels[2]] <- 1
  }
  
  else{
    Y[Y == -1] <- levels[1]
    Y[Y == 1] <- levels[2]
    Y <- as.factor(Y)
  }
  
  return(Y)
}

# we take the weak learner from rpart and add a weight variable
classTree <-  function(formula, data, w , depth , method = "class") {
  
  environment(formula) <- environment()

  rpart(formula,
        data,
        weights = w,
        method = method,
        control = rpart.control(maxdepth = depth))

  }


adaBoost <- function(formula, data, depth, noTrees, type = "train", test.data = NULL) {
  
  #checking that the inputs and parameters are in the correct format
  library(assertthat)
  library(rpart)
  assert_that(type %in% c("train", "predict"));
  not_empty(data)
  not_empty(formula)
  is.count(depth)
  is.count(noTrees)
  is.string(type)
  
  #extracting data from the input model, rearranging it in the correct format
  m.frame <- model.frame(formula, data)
  names <- colnames(m.frame)
  Terms <-  attr(m.frame, "terms")
  Xcoln <-  attr(Terms, "term.labels")

  Y <- model.response(m.frame)
  X <- subset(m.frame,select = Xcoln)
  
  levels <- levels(Y)
  Y <- regFunction(Y)
  
  data <-  data.frame(Y,X)
  colnames(data) <- names

  #creating a weights matrix and initializing adaboost variables  
  weightMatrix <-  matrix(1/nrow(X),
                        nrow = noTrees,
                        ncol = nrow(X))
  
  predictions <- matrix(0,
                        nrow = noTrees,
                        ncol = nrow(X))
  
  alpha <- rep(0, noTrees)
  error <- rep(0, noTrees)
  
  if(type == "predict"){
    test.predictions = matrix(0,
                              nrow = noTrees,
                              ncol = nrow(test.data))
  }
  
  #filling the weights matrix and running the algorithm
  for(i in 1:noTrees){
    
    current_weight <- weightMatrix[i,]
    
    fit.tree <- classTree(formula,
                            data,
                            current_weight,
                            depth)
    
    predictions[i,] <- as.numeric(as.character(predict(fit.tree, type = "class")))
    
    if(type == "predict"){
      test.predictions[i,] = as.numeric(as.character(predict(fit.tree, newdata = test.data, type = "class")))
    }
    
    indicator_function <- as.numeric(predictions[i,] != Y)
    
    error[i] <- t(current_weight) %*% indicator_function / sum(current_weight)
    alpha[i] <- log((1 - error[i])/error[i])
    
    if(i < noTrees){
      weightMatrix[i+1,] <- current_weight * exp(alpha[i] * indicator_function)
    }
  }
  
  #preparing the output
  predLabels <-  regFunction(sign(alpha %*% predictions),
                             rev = T,
                             levels)

  testLabels <- regFunction(sign(alpha %*% test.predictions),
                            rev = T,
                            levels)
  
  accuracy <-  mean(predLabels == model.response(m.frame))
  
  if(type == "predict"){
    return(list(predictionLabels = predLabels,
                testLabels = testLabels,
                accuracy = accuracy))
  }
  
  else{
    return(list(predictionLabels = predLabels,
                accuracy = accuracy)) 
  }
}

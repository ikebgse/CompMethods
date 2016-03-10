library(rpart)
library(ggplot2)
library(pbkrtest)
library(caret)
library(adabag)

#reading data
data <- read.csv(paste("C:/Users/MF/Documents/BGSE/TERM 2/Advanced Computational Methods/problem sets/PS6/",
                           "spambase.data",
                           sep=""), 
                     header=FALSE)

source(paste("C:/Users/MF/Documents/BGSE/TERM 2/Advanced Computational Methods/problem sets/PS6/",
             "adaBoost.R",
             sep=""))

data$V58 <- as.factor(data$V58)

#generating training and test data
train.ind = sample(1:nrow(data),3000)
train.data = data[train.ind,]
test.data = data.frame(data[-train.ind, -ncol(data)])

colnames(test.data) = colnames(train.data)[-ncol(data)]

depth = seq(1,10)

#initiatilizing error vectors for both the custom and the package adaboost
trainingError = rep(NA,length(depth))
testingError = rep(NA,length(depth))

package_trainingError = rep(NA,length(depth))
package_testingError = rep(NA,length(depth))

#growing trees
for(k in depth){
  
  custom.adaBoost <- adaBoost(formula = V58 ~ . , data = train.data , depth = k ,
                         noTrees = 10 , type = "predict", test.data = test.data)
  
  trainingError[k] <- 1 - mean(custom.adaBoost$predLabels == train.data[,c("V58")])
  testingError[k] <- 1 - mean(custom.adaBoost$testLabels == data[-train.ind,c("V58")])
  
  fit <- boosting(formula = V58 ~ .,
                  data = train.data,
                  mfinal = 10,
                  control = rpart.control(maxdepth = k))
  
  pkg.train <- as.factor(fit$class)
  
  boostPred.test <- predict(fit,newdata = test.data)
  pkg.test <- as.factor(boostPred.test$class)
  
  package_trainingError[k] <- 1 - mean(pkg.train == train.data[,c("V58")])
  package_testingError[k] <- 1 - mean(pkg.test == data[-train.ind,c("V58")])
}

#plotting
df <- data.frame(depth = depth,
                 trainingError,
                 testingError,
                 package_trainingError,
                 package_testingError)

df.melt <- melt(df,
                1,
                variable.name = "type",
                value.name = "error")

error_plot <- ggplot(data = df.melt, aes(x = depth, y = error, color = type)) + 
            geom_line() + 
            geom_point() +
            xlab("Tree depth") +
            ylab("Misclassification Error") +
            theme_bw(base_size = 12, base_family = "Arial")

#printing
cairo_pdf("adaBoost.pdf")
print(error_plot) 
dev.off()

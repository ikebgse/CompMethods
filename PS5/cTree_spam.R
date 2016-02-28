#setting workdir and reading data
#setwd("C:/Users/MF/Documents/BGSE/TERM 2/Advanced Computational Methods/problem sets/PS5")
#spambase <- read.csv("C:/Users/MF/Documents/BGSE/TERM 2/Advanced Computational Methods/problem sets/PS5/spambase.data", header=FALSE)

#loading required libraries
library(compiler)
library(zoo)
library(rpart)
library(reshape2)
library(assertthat)

#defining the data
data = spambase
nrow = nrow(data)

#creating a training sample
train.ind = sample(1:nrow,3000)
train.data = data[train.ind,]
test.data = data.frame(data[-train.ind,-ncol(data)])
colnames(test.data) = colnames(train.data)[-ncol(data)]

#speeding up the threshold finding function
findThreshold <- cmpfun(findThreshold)

#setting the depths that will appear on the final graph
depths = seq(1,15)

#initializing training and testing error vectors
my.train.err = rep(NA,length(depths))
my.test.err = rep(NA,length(depths))

pkg.train.err = rep(NA,length(depths))
pkg.test.err = rep(NA,length(depths))

#growing trees
for(k in depths){
  
  my.tree = cTree(formula = V58 ~ .,
                  data = train.data ,
                  depth = k, 
                  minPoints = 300,
                  costFnc = ME,
                  type = "predict",
                  test.data = test.data)
  
  my.train.err[k] = 1 - mean(my.tree$predLabels == train.data[,c("V58")])
  my.test.err[k] = 1 - mean(my.tree$testLabels == data[-train.ind,c("V58")])
  
    
  fit <- rpart(formula = V58 ~ .,
               data = train.data,
               method = "class",
               control=rpart.control(maxdepth=k,minsplit= 300))
  
  pkg.train = predict(fit, type = "class")
  pkg.test = predict(fit, newdata = test.data,type = "class")
  
  pkg.train.err[k] = 1 - mean(pkg.train == train.data[,c("V58")])
  pkg.test.err[k] = 1 - mean(pkg.test == data[-train.ind,c("V58")])
  
}

#rearranging and plotting the data
df <- data.frame(depth = depths,
                 training_error = my.train.err,
                 test_error = my.test.err,
                 package_training_error = pkg.train.err,
                 package_test_error = pkg.test.err)

df.melt <- melt(df,1,variable.name = "type",value.name = "error")

#defining plot variables
ErrorPlot <- ggplot(data = df.melt,
                  aes(x = depth, y = error, color = type)) + 
                  geom_line() + 
                  geom_point() +
                  xlab("Tree Depth") +
                  ylab("Misclassification Error") +
                  theme_bw(base_size = 14, base_family = "Arial")

# saving
cairo_pdf("cTree Error Plot.pdf")
print(ErrorPlot) 
dev.off()

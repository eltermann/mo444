#Input
#feature vectors
#observation.data - the data we gathered
#mean.y and sd.y are related to the normalization of observation.
#mean is mean
#sd is standard deviation
mo444LinearRegression <- function() {
  library('MASS')
  num.tweets <- nrow(feature.vectors)
  
  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)
  
  feature.vectors.train <- feature.vectors[1:train.size,,drop=F]
  feature.vectors.validation <- feature.vectors[(train.size+1):num.tweets,,drop=F]
  
  observation.data.train <- observation.data[1:train.size]
  observation.data.validation <- observation.data[(train.size+1):num.tweets]
  
  #theta - parameters
  trained.params <- ginv(feature.vectors.train) %*% observation.data.train
  estimated.results.train <- as.vector(feature.vectors.train %*% trained.params)
  estimated.results.validation <- as.vector(feature.vectors.validation %*% trained.params)
  
  # calculate errors
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  
  ret <- list()
  ret[['trained.params']] <- trained.params
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  
  return(ret)
}

mo444DecisionTree <- function() {
  library('rpart')
  
  num.tweets <- nrow(feature.vectors)
  
  train.size <- floor(0.8 * num.tweets)

  feature.vectors.df <- rand.data[,2:71] # exclude post.id column
  feature.vectors.train <- feature.vectors.df[1:train.size,,drop=F]
  feature.vectors.validation <- feature.vectors.df[(train.size+1):num.tweets,,drop=F]
    
  # Build decision tree and make predictions
  control <- rpart.control(xval=10, minsplit=4, minbucket=2, cp=0)
  #fit <- rpart(observation.data~retweets10sec+retweets20sec+retweets30sec+retweets40sec+retweets50sec+retweets60sec+retweets90sec+retweets02min+retweets03min+retweets04min+retweets05min+retweets06min+retweets07min+retweets08min+retweets09min+retweets10min+retweets15min+retweets20min+retweets25min+retweets30min+retweets60min+retweets90min+retweets2h+retweets3h+retweets4h+retweets5h+retweets6h+retweets10h+followers10sec+followers20sec+followers30sec+followers40sec+followers50sec+followers60sec+followers90sec+followers02min+followers03min+followers04min+followers05min+followers06min+followers07min+followers08min+followers09min+followers10min+followers15min+followers20min+followers25min+followers30min+followers60min+followers90min+followers2h+followers3h+followers4h+followers5h+followers6h+followers10h+week.sunday+week.monday+week.tuesday+week.wednesday+week.thursday+week.friday+week.saturday+hour.0to4+hour.4to8+hour.8to12+hour.12to16+hour.16to20+hour.20to24, data=feature.vectors.train, method='anova', control=control)
  fit <- rpart(observation.data~retweets10sec+retweets20sec+retweets30sec+retweets40sec+retweets50sec+retweets60sec+retweets90sec+retweets02min+retweets03min+retweets04min+retweets05min+followers10sec+followers20sec+followers30sec+followers40sec+followers50sec+followers60sec+followers90sec+followers02min+followers03min+followers04min+followers05min+week.sunday+week.monday+week.tuesday+week.wednesday+week.thursday+week.friday+week.saturday+hour.0to4+hour.4to8+hour.8to12+hour.12to16+hour.16to20+hour.20to24, data=feature.vectors.train, method='anova', control=control)
  #plot(fit, uniform=T)
  #text(fit, use.n = TRUE, cex = 0.75)

  estimated.results.train <- as.vector(predict(fit, newdata=feature.vectors.train))
  estimated.results.validation <- as.vector(predict(fit, newdata=feature.vectors.validation))

  observation.data.train <- observation.data[1:train.size]
  observation.data.validation <- observation.data[(train.size+1):num.tweets]

  # calculate error
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  
  ret <- list()
  ret[['fit']] <- fit
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  
  return(ret)
}


# Build feature.vectors
rand.data <- data[sample(41514),]
feature.vectors <- data.matrix(rand.data[,2:70])
observation.data <- rand.data[,'observed.result']

# Execute analysis
#regression.results <- mo444LinearRegression()
decision.tree.results <- mo444DecisionTree()
#Input
#feature vectors
#observation.data - the data we gathered
#mean.y and sd.y are related to the normalization of observation.
#mean is mean
#sd is standard deviation
mo444LinearRegression <- function(use.grouped=FALSE) {
  library('MASS')
  num.tweets <- nrow(feature.vectors)
  
  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)

  if (use.grouped) {
    feature.vectors.train <- feature.vectors.grouped[1:train.size.grouped,,drop=F]
    feature.vectors.validation <- feature.vectors.grouped[(train.size.grouped+1):num.tweets.grouped,,drop=F]
    observation.data.train <- observation.data.grouped[1:train.size.grouped]
    observation.data.validation <- observation.data.grouped[(train.size.grouped+1):num.tweets.grouped]
  }
  else {
    feature.vectors.train <- feature.vectors[1:train.size,,drop=F]
    feature.vectors.validation <- feature.vectors[(train.size+1):num.tweets,,drop=F]
    observation.data.train <- observation.data[1:train.size]
    observation.data.validation <- observation.data[(train.size+1):num.tweets]
  }
  
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
  
  feature.vectors.train <- data.frame.norm[1:train.size,,drop=F]
  feature.vectors.validation <- data.frame.norm[(train.size+1):num.tweets,,drop=F]
    
  # Build decision tree and make predictions
  control <- rpart.control(xval=10, minsplit=4, minbucket=2, cp=0)
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

load("/home/felipe/2s2013/mo444/mo444/analysis/data.Rdata")
# Build feature.vectors
rand.data <<- data[sample(41514),]
feature.vectors <<- data.matrix(rand.data[,2:70])
observation.data <<- rand.data[,'observed.result']

# Group feature vectors with same Y (observed.result)
library('plyr')
feature.vectors.grouped.df <<- ddply(rand.data, ~observed.result, summarise,retweets10sec=mean(retweets10sec),retweets20sec=mean(retweets20sec),retweets30sec=mean(retweets30sec),retweets40sec=mean(retweets40sec),retweets50sec=mean(retweets50sec),retweets60sec=mean(retweets60sec),retweets90sec=mean(retweets90sec),retweets02min=mean(retweets02min),retweets03min=mean(retweets03min),retweets04min=mean(retweets04min),retweets05min=mean(retweets05min),retweets06min=mean(retweets06min),retweets07min=mean(retweets07min),retweets08min=mean(retweets08min),retweets09min=mean(retweets09min),retweets10min=mean(retweets10min),retweets15min=mean(retweets15min),retweets20min=mean(retweets20min),retweets25min=mean(retweets25min),retweets30min=mean(retweets30min),retweets60min=mean(retweets60min),retweets90min=mean(retweets90min),retweets2h=mean(retweets2h),retweets2h=mean(retweets2h),retweets3h=mean(retweets3h),retweets4h=mean(retweets4h),retweets5h=mean(retweets5h),retweets6h=mean(retweets6h),retweets10h=mean(retweets10h),followers10sec=mean(followers10sec),followers20sec=mean(followers20sec),followers30sec=mean(followers30sec),followers40sec=mean(followers40sec),followers50sec=mean(followers50sec),followers60sec=mean(followers60sec),followers90sec=mean(followers90sec),followers02min=mean(followers02min),followers03min=mean(followers03min),followers04min=mean(followers04min),followers05min=mean(followers05min),followers06min=mean(followers06min),followers07min=mean(followers07min),followers08min=mean(followers08min),followers09min=mean(followers09min),followers10min=mean(followers10min),followers15min=mean(followers15min),followers20min=mean(followers20min),followers25min=mean(followers25min),followers30min=mean(followers30min),followers60min=mean(followers60min),followers90min=mean(followers90min),followers2h=mean(followers2h),followers2h=mean(followers2h),followers3h=mean(followers3h),followers4h=mean(followers4h),followers5h=mean(followers5h),followers6h=mean(followers6h),followers10h=mean(followers10h),week.sunday=mean(week.sunday),week.monday=mean(week.monday),week.tuesday=mean(week.tuesday),week.wednesday=mean(week.wednesday),week.thursday=mean(week.thursday),week.friday=mean(week.friday),week.saturday=mean(week.saturday),hour.0to4=mean(hour.0to4),hour.4to8=mean(hour.4to8),hour.8to12=mean(hour.8to12),hour.12to16=mean(hour.12to16),hour.16to20=mean(hour.16to20),hour.20to24=mean(hour.20to24))
feature.vectors.grouped <<- data.matrix(feature.vectors.grouped.df[,2:70])
observation.data.grouped <<- feature.vectors.grouped.df[,1]


# Normalization
num.tweets <<- nrow(feature.vectors)
train.size <<- floor(0.8 * num.tweets)
means.train <<- apply(feature.vectors[1:train.size,],2,mean)
sds.train <<- apply(feature.vectors[1:train.size,], 2, sd)
feature.vectors.norm <<- t(apply(feature.vectors, 1, function(x) { x-means.train })) # subtract each feature value by feature mean
feature.vectors.norm <<- t(apply(feature.vectors.norm, 1, function(x) { x/sds.train }))  # divide each row by its standard deviation
feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias
data.frame.norm <<- data.frame(feature.vectors.norm, observation.data)

num.tweets.grouped <<- nrow(feature.vectors.grouped)
train.size.grouped <<- floor(0.8 * num.tweets.grouped)

# Execute analysis
regression.results <- mo444LinearRegression(use.grouped=T)
#decision.tree.results <- mo444DecisionTree()

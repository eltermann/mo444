library('RMySQL')
library('MASS')

#Input
#elapsed time <-vector of time elapsed in seconds
#count - number of retweets
mo444GetFeatureVector <- function(elapsed_time, count) {
  feature.vector <- array(0, dim=length(times))
  i <- 1
  index <- 1
  for (index in 1:length(times)) {
    for (j in i:length(elapsed_time)) {
      if (times[index] < elapsed_time[j]) {
        break
      }
    }

    i <- j
    if (i == 1) {
      # (left boundary exception) no retweets until this "time"
      feature.vector[index] <- 0
    }
    else if (j == length(elapsed_time)) {
      # (right boundary exception) "time" larger than last retweet time
      feature.vector[index] <- count[i]
    }
    else {
      feature.vector[index] <- ifelse(i == 1, 0, count[i-1])
    }
  }

  return(feature.vector)
}


#Input
#feature vectors
#observation.data - the data we gathered
#mean.y and sd.y are related to the normalization of observation.
#mean is mean
#sd is standard deviation
mo444LinearRegression <- function() {
  num.tweets <- nrow(feature.vectors)

  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)

  feature.vectors.train <- feature.vectors.norm[1:train.size,,drop=F]
  feature.vectors.validation <- feature.vectors.norm[(train.size+1):num.tweets,,drop=F]

  observation.data.norm <<- observation.data.norm[1:num.tweets]
  observation.data.train <- observation.data.norm[1:train.size]
  observation.data.validation <- observation.data.norm[(train.size+1):num.tweets]

  #theta - parameters
  trained.params <- ginv(feature.vectors.train) %*% observation.data.train
  estimated.results.train <- as.vector(feature.vectors.train %*% trained.params)
  estimated.results.validation <- as.vector(feature.vectors.validation %*% trained.params)

  # unnormalize output and calculate error
  observation.data.train.un <- (observation.data.train * sd.y) + mean.y
  observation.data.validation.un <- (observation.data.validation * sd.y) + mean.y
  estimated.results.train.un <- (estimated.results.train * sd.y) + mean.y
  estimated.results.validation.un <- (estimated.results.validation * sd.y) + mean.y

  J.train <- mean((estimated.results.train.un - observation.data.train.un)^2)
  J.validation <- mean((estimated.results.validation.un - observation.data.validation.un)^2)

  ret <- list()
  ret[['trained.params']] <- trained.params
  ret[['observation.data.train']] <- observation.data.train.un
  ret[['estimated.results.train']] <- estimated.results.train.un
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation.un
  ret[['estimated.results.validation']] <- estimated.results.validation.un
  ret[['J.validation']] <- J.validation

  return(ret)
}

con <- dbConnect(MySQL(), user="root", dbname="mo444-twitter")

times <<- c(
  10, 20,30, 40,50, # 10 to 50 seconds
  1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
  15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
  1*60*60, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
)

#followed.users <- read.csv('/home/felipe/2s2013/mo444/mo444/analysis/followed_user_ids.csv', header=FALSE)
#for (u in 1:nrow(followed.users)) {

  #user.name <- as.character(followed.users[u,'V1'])
  #user.id <- as.character(followed.users[u,'V2'])


  #print(paste(user.name, user.id, sep=': '))

  #q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id FROM tweet_with_created_at2 t WHERE t.user_id = %s AND t.tweet_created_at > 1379827567 AND t.tweet_created_at < 1379937600;", user.id))
  print('fetching tweets...')
  q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id, FROM_UNIXTIME(t.tweet_created_at, '%%w;%%H') AS week_and_hour FROM tweet_with_created_at2 t WHERE t.user_id IN (27260086,21447363,14230524,813286,17919972,10228272,16409683,79293791,180505807,26565946,101402940,783214,85603854,44409004,15846407,155659213,19397785,28706024,25365536,21111883,184910040,1311017701,35787166,100220864,60865434,23375688,35094637,181561712,22940219,209708391,19058681,105119490,815322103,31927467,268414482,116362700,19248106,428333,73992972,119509520,85426644,158314798,84279963,1311017701,50393960,23976386,27195114,24929621,31239408,2425151,52551600,20322929,16190898,18863815,218571680,3004231,21425125,18091904,53153263,176566242,43152482,259379883,759251) AND t.tweet_created_at > 1379827567 AND t.tweet_created_at < 1379937600;"))
  if (!'tweet_id' %in% names(q0)) {
    print('not enough tweets')
    next
  }
  user_tweets <- q0[['tweet_id']]

  feature.vectors <<- matrix(0, nrow=length(user_tweets), ncol=4*length(times))
  categoric.feature.vectors <<- matrix(0, nrow=length(user_tweets), ncol=2)
  observation.data <<- array(0, dim=length(user_tweets))
  tweet_counter <- 1
  print('fetching retweets history...')
  for (tweet_index in 1:length(user_tweets)) {
    tweet_id <- as.character(q0[tweet_index,'tweet_id'])
    tweet_week <- substr(q0[tweet_index,'week_and_hour'], 1, 1)
    tweet_hour <- substr(q0[tweet_index,'week_and_hour'], 3, 4)
    q <- dbGetQuery(con, sprintf("SELECT * FROM retweets_history4 WHERE original_tweet_id = %s;", tweet_id))
    q2 <- dbGetQuery(con, sprintf("SELECT t.tweet_text, u.user_screen_name FROM tweet t INNER JOIN user u ON t.user_id = u.user_id where t.tweet_id = %s;", tweet_id))

    if ('elapsed_time' %in% names(q) && length(q[['elapsed_time']]) > 0) {
      observation.data[tweet_counter] <- length(q[['elapsed_time']])
      feature.vectors[tweet_counter,] <- c(
         mo444GetFeatureVector(q[['elapsed_time']], q[['count']]),
         mo444GetFeatureVector(q[['elapsed_time']], q[['reached_followers_count']]),
         mo444GetFeatureVector(q[['elapsed_time']], q[['reached_friends_count']]),
         mo444GetFeatureVector(q[['elapsed_time']], q[['reached_favorited_count']])
      )
      categoric.feature.vectors[tweet_counter,] <- c(tweet_week, tweet_hour)
      tweet_counter <- tweet_counter + 1
    }
  }
  num.tweets <- tweet_counter - 1
  feature.vectors <<- feature.vectors[1:num.tweets,]
  categoric.feature.vectors <<- categoric.feature.vectors[1:num.tweets,]

  if (!is.matrix(feature.vectors) || nrow(feature.vectors) <= 1) {
    print('no tweets found.')
    next
  }
  print(paste('num posts: ', num.tweets, sep=''))

  num.tweets <- nrow(feature.vectors)
  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)

  # normalization
  means.train <- apply(feature.vectors[1:train.size,,drop=F],2,mean)
  sds.train <- apply(feature.vectors[1:train.size,,drop=F], 2, sd)
  feature.vectors.norm <<- t(apply(feature.vectors, 1, function(x) { x-means.train })) # subtract each feature value by feature mean
  feature.vectors.norm <<- t(apply(feature.vectors.norm, 1, function(x) { x/sds.train }))  # divide each row by its standard deviation
  feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias

  mean.y <<- mean(observation.data[1:train.size])
  sd.y <<- sd(observation.data[1:train.size])
  observation.data.norm <<- (observation.data - mean.y) / sd.y

  regression.results <- mo444LinearRegression()

  print(regression.results)
#}

dbDisconnect(con)

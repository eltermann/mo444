library("RMySQL")


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

con <- dbConnect(MySQL(), user="root", dbname="mo444-twitter")

times <<- c(
  10, 20,30, 40,50, # 10 to 50 seconds
  1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
  15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
  1*60*60, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
)

#user_id <- 813286 # @BarackObama
user_id <- 742143 # @BBCWorld


q0 <- dbGetQuery(con, sprintf("SELECT DISTINCT(history.original_tweet_id) FROM retweets_history2 history INNER JOIN tweet_with_created_at2 t ON history.original_tweet_id = t.tweet_id WHERE t.user_id = %s AND t.tweet_created_at > 1379827567;", user_id))
user_tweets <- q0[['original_tweet_id']]

feature.vectors <- matrix(0, nrow=length(times), ncol=length(user_tweets))
tweet_counter <- 1
for (tweet_id in as.character(user_tweets)) {
  q <- dbGetQuery(con, sprintf("SELECT * FROM retweets_history2 WHERE original_tweet_id = %s;", tweet_id))
  q2 <- dbGetQuery(con, sprintf("SELECT t.tweet_text, u.user_screen_name FROM tweet t INNER JOIN user u ON t.user_id = u.user_id where t.tweet_id = %s;", tweet_id))
  #plot(q[['elapsed_time']]/60, q[['count']], type='o', cex=0.4, xlab="Time (minutes)", ylab="# RT", main=paste('@', q2[['user_screen_name']], sep=''))

  if ('elapsed_time' %in% names(q) && length(q[['elapsed_time']]) > 0) {
    feature.vector <- mo444GetFeatureVector(q[['elapsed_time']], q[['count']])
    feature.vectors[,tweet_counter] <- feature.vector
    tweet_counter <- tweet_counter + 1
  }
}
feature.vectors <- feature.vectors[,1:(tweet_counter-1)]

print(feature.vectors)



#### Aux methods


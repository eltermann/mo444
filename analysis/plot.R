library("RMySQL")

con <- dbConnect(MySQL(), user="root", dbname="mo444-twitter")

#tweet_id <- 381616468934557696
tweet_id <- 381688205981913088
q <- dbGetQuery(con, sprintf("SELECT * FROM retweets_history2 WHERE original_tweet_id = %s;", tweet_id))
q2 <- dbGetQuery(con, sprintf("SELECT t.tweet_text, u.user_screen_name FROM tweet t INNER JOIN user u ON t.user_id = u.user_id where t.tweet_id = %s;", tweet_id))

#plot(history[['elapsed_time']]/60, history[['count']], type='o', cex=0.4, xlab="Time (minutes)", ylab="# RT", main=paste('@', q2[['user_screen_name']], sep=''))


times <- c(
  10, 20,30, 40,50, # 10 to 50 seconds
  1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
  15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
  1*60*60, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
)

feature.vector <- array(0, dim=length(times))
i <- 1
index <- 1
for (index in 1:length(times)) {
  for (j in i:length(q[['elapsed_time']])) {
    if (times[index] < q[['elapsed_time']][j]) {
      break
    }
  }

  i <- j

  if (i == 1) {
    # (left boundary exception) no retweets until this "time"
    feature.vector[index] <- 0
  }
  else if (j == length(q[['elapsed_time']])) {
    # (right boundary exception) "time" larger than last retweet time
    feature.vector[index] <- q[['count']][i]
  }
  else {
    feature.vector[index] <- ifelse(i == 1, 0, q[['count']][i-1])
  }
}

print(feature.vector)

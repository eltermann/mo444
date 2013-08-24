class Fetcher():
   def __init__(self, api=None, db=None):
      self.api = api
      self.db = db

   def fetchTweetRecursive(self, tweetId, maxLevels = 0):
      """DFS on retweets tree of a given tweet. A maximum number of levels may be *optionally* given."""

      if maxLevels < 0:
         return

      tweet = self.api.GetStatus(tweetId)
      self.visitTweet(tweet)

      if maxLevels == 1:
         return

      retweets = self.api.GetRetweets(tweetId, count=100)
      if retweets.count > 0:
         newMaxLevels = 0 if maxLevels == 0 else maxLevels - 1
         for retweet in retweets:
            self.fetchTweetRecursive(retweet.id, newMaxLevels)


   def visitTweet(self, tweet):
      print "visiting tweet %d" % (tweet.id)
      '''
      TODO check whether tweet is already persisted
      TODO if not, persist it
      '''

import json
import MySQLdb
import os

class TweetsPersister():
   def __init__(self):
      basepath = os.path.dirname(os.path.realpath(__file__))
      json_fp = open(basepath + '/db-credentials.json')
      credentials = json.load(json_fp)
      self.db_raw = MySQLdb.connect(host = credentials['raw-tweets']['host'],
                                    db = credentials['raw-tweets']['db'],
                                    user = credentials['raw-tweets']['user'],
                                    passwd = credentials['raw-tweets']['passwd'])

      self.db = MySQLdb.connect(host = credentials['structured-tweets']['host'],
                                db = credentials['structured-tweets']['db'],
                                user = credentials['structured-tweets']['user'],
                                passwd = credentials['structured-tweets']['passwd'])


   def insertRawTweet(self, string):
      c = self.db_raw.cursor()
      c.execute("INSERT INTO raw_tweets (tweet) VALUES (%s)", string)
      self.db_raw.commit()


   def loadTweet(self, tweet_id):
      """
      Tweet loader.
      @param tweet_id
      @return Populated tweet dictionary, if tweet found. "None" otherwise.
      """
      c = self.db.cursor()
      c.execute("SELECT tweet_text, tweet_source, user_id, tweet_retweeted_status_id FROM tweet WHERE tweet_id = %s", tweet_id)
      row = c.fetchone()
      if (row is None):
         return None

      tweet = {
         'id': tweed_id,
         'text': row[0],
         'source': row[1],
         'user_id': row[2],
         'retweeted_status_id': row[3],
      }

      return tweet


   def loadUser(self, user_id):
      """
      User loader.
      @param user_id
      @return Populated user dictionary, if found. "None" otherwise.
      """
      c = self.db.cursor()
      c.execute("SELECT user_name, user_screen_name, user_location, user_description, user_url, user_followers_count, user_friends_count FROM user WHERE user_id = %s", user_id)
      row = c.fetchone()
      if (row is None):
         return None

      user = {
         'id': user_id,
         'name': row[0],
         'screen_name': row[1],
         'location': row[2],
         'description': row[3],
         'url': row[4],
         'followers_count': row[5],
         'friends_count': row[6],
      }

      return user

   def saveTweet(self, tweet):
      # TODO
      return

   def saveUser(self, user):
      # TODO
      return

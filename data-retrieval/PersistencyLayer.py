import json
import MySQLdb

class TweetsPersister():
   def __init__(self):
      json_fp = open('./db-credentials.json')
      credentials = json.load(json_fp)
      self.db_raw = MySQLdb.connect(host = credentials['raw-tweets']['host'],
                                    db = credentials['raw-tweets']['db'],
                                    user = credentials['raw-tweets']['user'],
                                    passwd = credentials['raw-tweets']['passwd'])


   def insertRawTweet(self, string):
      c = self.db_raw.cursor()
      c.execute("INSERT INTO raw_tweets VALUES (%s)", string)
      self.db_raw.commit()

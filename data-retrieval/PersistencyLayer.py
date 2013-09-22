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


   def insertRawTweet(self, string):
      c = self.db_raw.cursor()
      c.execute("INSERT INTO raw_tweets VALUES (%s)", string)
      self.db_raw.commit()

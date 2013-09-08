import json
import MySQLdb

class RawTweetsPersister():
   def __init__(self):
      json_fp = open('./db-credentials.json')
      credentials = json.load(json_fp)
      print credentials
      self.db = MySQLdb.connect(host = credentials['raw-tweets']['host'],
                           db = credentials['raw-tweets']['db'],
                           user = credentials['raw-tweets']['user'],
                           passwd = credentials['raw-tweets']['passwd'])


   def insertRawTweet(self, string):
      c = self.db.cursor()
      ret = c.execute("INSERT INTO raw_tweets VALUES ('%s')" % string)
      print ret

class Persister():
   def __init__(self):
      return

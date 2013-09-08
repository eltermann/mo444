import MySQLdb

class RawTweetsPersister():
   json_fp = open('./db-credentials.json')
   credentials = json.load(json_fp)
   db = MySQLdb.connect(host = credentials['raw-tweets']['host'],
                        db = credentials['raw-tweets']['db'],
                        user = credentials['raw-tweets']['user'],
                        passwd = credentials['raw-tweets']['passwd'])

   def __init__(self):
      return


   def insertRawTweet(self, string):
      c = db.cursor()
      c.execute("INSERT INTO raw_tweets VALUES (?)", (string))

class Persister():
   def __init__(self):

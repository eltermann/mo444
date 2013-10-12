import json
import PersistencyLayer

persister = PersistencyLayer.TweetsPersister()

last_raw_id = 0
step = 100000

c1 = persister.db_raw.cursor()

for offset in range(0, 99999999, step):
   c1.execute("SELECT raw_id, tweet FROM raw_tweets WHERE raw_id >= %s LIMIT %s,%s", (last_raw_id, offset, step))
   row = c1.fetchone()
   while not row is None:
      print "raw_id: %s" % row[0]
      data = json.loads(row[1])
      if 'retweeted_status' in data and 'id' in data['retweeted_status']:
         # save retweeted status
         retweeted = persister.saveTweet(data['retweeted_status'])
         #data['retweeted_status_id'] = retweeted['id']
      else:
         data['retweeted_status_id'] = 0

      # now, save tweet itself
      persister.saveTweet(data)

      # get next raw tweet
      row = c1.fetchone()

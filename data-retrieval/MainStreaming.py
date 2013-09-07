import json
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

json_fp = open('./twitter-api-credentials.json')
credentials = json.load(json_fp)
followedUserId = '813286'

class StdOutListener(StreamListener):
   """ A listener handles tweets are the received from the stream.
   This is a basic listener that just prints received tweets to stdout.

   """
   def on_data(self, data):
      parsed = json.loads(data)

      # we are only interested on user's tweets or direct retweets
      if parsed['user']['id_str'] == followedUserId or ('retweeted_status' in parsed and parsed['retweeted_status']['user']['id_str'] == followedUserId):
         print data

      return True

   def on_error(self, status):
      return

if __name__ == '__main__':
   l = StdOutListener()
   auth = OAuthHandler(credentials['consumer_key'], credentials['consumer_secret'])
   auth.set_access_token(credentials['access_token_key'], credentials['access_token_secret'])

   stream = Stream(auth, l)
   stream.filter(follow=[followedUserId])

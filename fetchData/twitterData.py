#from twython import Twython, TwythonError
#!/usr/bin/python

import tweepy
import csv #Import csv
import os

consumer_key= 'pDAqKvD6VXFBBF8uNXOu1gPMG'
consumer_secret  = 'E7T5OgiP3Kn0fa3w9h4X5dJVDK0JY18uBmS4wNp8nS5jxVezO4'
access_token = '493021440-Z8n3VYoKW6mmZ5eIyskOEgTMP3h9VxKO51UVsn6e'
access_token_secret = 'Q6CqSya8BTeF4ntC7dvxTJD1XW6UbnMGelcUAodh5eryc'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)


api = tweepy.API(auth)
csvFile = open('docker1.csv', 'a')
csvWriter = csv.writer(csvFile)
ids = set()
for tweet in tweepy.Cursor(api.search, q="#GalaxyS8",  Since="2017-01-01", until="2017-05-25",lang="en").items(10000):

    if (tweet.retweeted) and ('RT @' in tweet.text):
        print ("hi")
        #Write a row to the csv file/ I use encode utf-8
        csvWriter.writerow([tweet.created_at, tweet.text.encode('utf-8'), tweet.favorite_count, tweet.retweet_count, tweet.id, tweet.user.screen_name])
        #print "...%s tweets downloaded so far" % (len(tweet.id))
        ids.add(tweet.id) # add new id
        print ("number of unique ids seen so far: {}",format(len(ids)))
        csvFile.close()
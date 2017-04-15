from __future__ import division
import pip
import pandas as pd
import subprocess
import time
import os
import json
import csv

start_time = time.time()

time.sleep(2)

if not os.path.exists("output"):
    os.makedirs("output")


def import_or_install(package):
    try:
        __import__(package)
        print ("Package already Installed : " + package + '\n')
    except ImportError:
        print ("Installing Package : " + package + '\n')
        pip.main(['install', package])


pack = ['shutil', 'csv', 'subprocess', 'urllib2', 'json', 'datetime', 'time', 'os', 'pandas', 'requests', 'argparse',
        'lxml', 'pyquery']
for i in range(0, len(pack)):
    pk = pack[i]
    import_or_install(pk)

df = pd.read_csv('params.csv', index_col=False)
df = df.fillna(0)
query = df.iat[0, 1]
fb = df.iat[1, 1]
twitterquery = df.iat[6, 1]
fromdate = df.iat[7, 1]
tilldate = df.iat[8, 1]
num = df.iat[9, 1]
videoid = df.iat[10, 1]

if query == 0:
    print("Please enter the Ad details")
else:
    outyou = query + "_youtube_1.txt"
    if fb == 0:
        print ("No Facebook")
    else:
        com1 = ('python' + ' ' + 'fetchFacebookStatus.py')
        print (com1)
        subprocess.Popen(com1).wait()

    if twitterquery == 0:
        print ("No Twitter")
    else:
        out = (query + "_tweets" + ".txt")
        com2 = (
            'python' + ' ' + 'fetchTweets.py' + ' ' + '--querysearch' + ' ' + twitterquery + ' ' + '--since' + ' ' + fromdate
            + ' ' + '--until' + ' ' + tilldate + ' ' + '--maxtweets' + ' ' + num + ' ' + '--filename' + ' ' + out)
        print (com2)
        subprocess.Popen(com2).wait()
        with open(out, 'r') as in_file:
            stripped = (line.strip() for line in in_file)
            lines = (line.split("|$") for line in stripped if line)
            with open("output\\" + query + '_Twitter_Tweets.csv', 'wb') as out_file:
                writer = csv.writer(out_file)
                writer.writerows(lines)
        os.remove(query+"_tweets.txt")

    if videoid == 0:
        print ("NO Youtube")
    else:
        print outyou
        com3 = ('python' + ' ' + 'fetchYouTubeComments.py' + ' ' + '-y' + ' ' + videoid + ' ' + '-o' + ' ' + outyou)
        print (com3)
        subprocess.Popen(com3).wait()
        out = (query+"_youtube" + ".txt")
        text_file = open(out, "a+")
        head = ("text" + '|$' + "author" + '|$' + "cid" + '|$' + "time")
        text_file.write(head + '\n')
        with open(outyou, "r") as f:
            for line in f:
                line = str(line)
                r = json.loads(line)
                res = (r["text"] + '|$' + r["author"] + '|$' + r["cid"] + '|$' + r["time"])
                res = ' '.join(res.splitlines())
                res = res.encode('utf-8')
                text_file.write(res + '\n')

        text_file.close()
        os.remove(outyou)

        with open(out, 'r') as in_file:
            stripped = (line.strip() for line in in_file)
            lines = (line.split("|$") for line in stripped if line)
            with open('output\\' + query + '_YouTube_Video_Comments.csv', 'wb') as out_file:
                writer = csv.writer(out_file)
                writer.writerows(lines)

        os.remove(out)
print ("Process Completed")

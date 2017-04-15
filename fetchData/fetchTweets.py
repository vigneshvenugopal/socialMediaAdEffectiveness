import sys
import getopt
import got
import codecs


def main(argv):
    global arg
    global outputFile

    if len(argv) == 0:
        print ('You must pass some parameters. Use \"-h\" to help.')
        return

    if len(argv) == 1 and argv[0] == '-h':
        return

    try:
        opts, args = getopt.getopt(argv, "",
                                   ("username=", "since=", "until=", "querysearch=", "toptweets", "maxtweets=", "filename="))

        tweetcriteria = got.manager.TweetCriteria()

        for opt, arg in opts:
            if opt == '--username':
                tweetcriteria.username = arg

            elif opt == '--since':
                tweetcriteria.since = arg

            elif opt == '--until':
                tweetcriteria.until = arg

            elif opt == '--querysearch':
                tweetcriteria.querySearch = arg

            elif opt == '--toptweets':
                tweetcriteria.topTweets = True

            elif opt == '--maxtweets':
                tweetcriteria.maxTweets = int(arg)

            elif opt == '--filename':
                filename = arg

        outputFile = codecs.open(filename, "w+", "utf-8")

        outputFile.write('username|$date|$retweets|$favorites|$text|$geo|$mentions|$hashtags|$id|$permalink')

        print ('Searching...\n')

        def receivebuffer(tweets):
            for t in tweets:
                outputFile.write(('\n%s|$%s|$%d|$%d|$"%s"|$%s|$%s|$%s|$"%s"|$%s' % (
                    t.username, t.date.strftime("%Y-%m-%d %H:%M"), t.retweets, t.favorites, t.text, t.geo, t.mentions,
                    t.hashtags, t.id, t.permalink)))
            outputFile.flush()
            print ('More %d saved on file...\n' % len(tweets))

        got.manager.TweetManager.getTweets(tweetcriteria, receivebuffer)

    except arg:
        print ('Arguments parser error, try -h' + arg)
    finally:
        outputFile.close()
        print ('Done. Output file generated ' + filename)


if __name__ == '__main__':
    main(sys.argv[1:])

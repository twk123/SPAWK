from __future__ import print_function
import sys
from operator import add
from pyspark import SparkConf, SparkContext
dataFile = "spawk.ml"  # Should be some file on your system
conf = (SparkConf()
         .setMaster("local")
         .setAppName("spawk")
         .set("spark.executor.memory", "1g"))

sc = SparkContext(conf = conf)
spawkData = sc.textFile(dataFile).cache()


numAs = spawkData.filter(lambda s: 'a' in s).count()
numBs = spawkData.filter(lambda s: 'b' in s).count()

print "Lines with a: %i, lines with b: %i" % (numAs, numBs)


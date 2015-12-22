from __future__ import print_function
import sys
import re
from operator import add
from pyspark import SparkConf, SparkContext
dataFile = "../wordcount_test.txt"  # Should be some file on your system
conf = (SparkConf()
         .setMaster("local")
         .setAppName("spawk")
         .set("spark.executor.memory", "1g"))

sc = SparkContext(conf = conf)



spawkData = sc.textFile(dataFile).cache()


def mapper(x):
    return (x,1)

def reducer(x, y):
    return x + y

flatMapStep = spawkData.flatMap(lambda x: x.split(' '))
mapStep = flatMapStep.map(mapper)
reduceStep = mapStep.reduceByKey(reducer)
A = reduceStep.collect()
for (k,v) in A:
   print(k, v)


sc.stop()


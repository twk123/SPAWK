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


def gcd(x, y):
    while (y != 0):
        temp = x ; x = y ; y = temp % y
    return x

print(gcd(40, 56))


sc.stop()


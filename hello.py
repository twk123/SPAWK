from __future__ import print_function
import sys
import re
from operator import add
from pyspark import SparkConf, SparkContext
dataFile = "spark.log"  # Should be some file on your system
conf = (SparkConf()
         .setMaster("local")
         .setAppName("spawk")
         .set("spark.executor.memory", "1g"))

sc = SparkContext(conf = conf)



spawkTempData = sc.textFile(dataFile).cache()
def parse_rrd_line(line):
    match = re.search('RDD',line)
    if match is None:
        return ''
    return line

spawkData = (spawkTempData.map(parse_rrd_line).cache())
print("lines in filtered RRD: %i" % spawkData.count())


a = 1 + 2



sc.stop()


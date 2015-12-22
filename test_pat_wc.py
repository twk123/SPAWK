import sys
import re
from pyspark import SparkContext
sc = SparkContext(appName="WordCountExample")
lines = sc.textFile(sys.argv[1])

def parse_rrd_line(line):
    match = re.search('RDD',line)
    if match is None:
        return ''
    return line

Data = (lines.map(parse_rrd_line).cache())

def mapper(x):
    return (x,1)
def reducer(x,y):
    return (x+y)
flatMapStep = Data.flatMap(lambda x: x.split(' '))
mapStep = flatMapStep.map(mapper) 
reduceStep = mapStep.reduceByKey(reducer)
var_tuple = reduceStep.collect()
for key in var_tuple:
    print("%s %i" % (key[0], key[1]))
    #print(key[0], key[1])
sc.stop()


import sys
import re
from pyspark import SparkContext
sc = SparkContext(appName="WordCountExample")
lines = sc.textFile(sys.argv[1])

def mapper(x):
    return (x,1)
def reducer(x,y):
    return (x+y)
flatMapStep = lines.flatMap(lambda x: x.split(' '))
mapStep = flatMapStep.map(mapper) 
reduceStep = mapStep.reduceByKey(reducer)
var_tuple = reduceStep.collect()
for key in var_tuple:
    print("%s %i" % (key[0], key[1]))
    #print(key[0], key[1])
sc.stop()


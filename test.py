import sys
from pyspark import SparkContext
sc = SparkContext(appName="WordCountExample")
lines = sc.textFile(sys.argv[1])
flatMapStep = lines.flatMap(lambda x: x.split(' '))
mapStep = flatMapStep.map(lambda x: (x, 1)) 
reduceStep = mapStep.reduceByKey(lambda x,y:x+y)
output = reduceStep.collect()
for (word, count) in output:
    print "%s: %i" % (word, count)
sc.stop()


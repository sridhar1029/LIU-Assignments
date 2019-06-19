#!/usr/bin/env python2
# -*- coding: utf-8 -*-

#Sridhar Adhikarla

import time
from pyspark import SparkContext

iFile = 'data/temperature-readings.csv'
oFile = 'outputs/out1_a'
fromYear = 1950
toYear = 2014

sc = SparkContext(appName="Lab1_Q1A_SparkJob")

start = time.time()

lines = sc.textFile(iFile)
lines = lines.map(lambda a: a.split(";"))
lines = lines.filter(lambda x: int(x[1][0:4]) >= 1950 and int(x[1][0:4]) <= 2014)
temperatures = lines.map(lambda x: (x[1][0:4], (x[0], float(x[3]))))

#Custom MIN/MAX Function 
my_min = (lambda x, y: x if x[1] < y[1] else y)
my_max = (lambda x, y: x if x[1] > y[1] else y)

minTemperatures = temperatures.reduceByKey(my_min)
maxTemperatures = temperatures.reduceByKey(my_max)
minMaxTemp = minTemperatures.union(maxTemperatures).reduceByKey(lambda x,y: (x[0],x[1],y[0],y[1]))

sortedMinMaxTemp = minMaxTemp.sortBy(ascending=False, keyfunc=lambda a: a[1][3])
sortedMinMaxTempCsv = sortedMinMaxTemp.map(lambda a: '%s,%s,%s,%s,%s' % (a[0], a[1][0], a[1][1], a[1][2], a[1][3]))
sortedMinMaxTempCsv.coalesce(1).saveAsTextFile(oFile)

end = time.time()
print('Script Lab1_Q1_A Done in %s seconds' % (end - start))

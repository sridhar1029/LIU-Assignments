#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/temperature-readings.csv'
oFile = 'outputs/out3'

sc = SparkContext(appName="Lab2_Q3_SparkSQLJob")

sqlContext = SQLContext(sc)

temperature_file = sc.textFile(iFile)
lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.filter(lambda x:(int(x[1][0:4]) >= 1960 and int(x[1][0:4]) <=2014))
temp = temp.map(lambda x: Row(station=x[0], year=int(x[1][0:4]), month = int(x[1][5:7]), temp = float(x[3])))

schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

avgTemp = schemaTempReadings.groupBy('year','month','station').agg(F.avg('temp').alias('avgtemp')).orderBy(['avgtemp'],ascending = False)

avgTemp.rdd.repartition(1).saveAsTextFile(oFile)
print(avgTemp.take(15))

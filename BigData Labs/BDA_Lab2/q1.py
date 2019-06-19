#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/temperature-readings.csv'
oFile1 = 'outputs/out1_1'
oFile2 = 'outputs/out1_2'

fromYear = 1950
toYear = 2014

sc = SparkContext(appName = "Lab2_Q1_SparkSQLJob")
sqlContext = SQLContext(sc)

temperature_file = sc.textFile(iFile)

lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.filter(lambda x:(int(x[1][0:4]) >= fromYear and int(x[1][0:4]) <=toYear))
temp = temp.map(lambda x: Row(station=x[0], year=int(x[1][0:4]), temp = float(x[3])))

schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

schemaTempReadingsMin = schemaTempReadings.groupBy('year','station').agg(F.min('temp').alias('mintemp')).orderBy(['mintemp'],descending=1)

schemaTempReadingsMax = schemaTempReadings.groupBy('year','station').agg(F.max('temp').alias('maxtemp')).orderBy(['maxtemp'], ascending = False)

schemaTempReadingsMin.rdd.repartition(1).saveAsTextFile(oFile1)
schemaTempReadingsMax.rdd.repartition(1).saveAsTextFile(oFile2)


print(schemaTempReadingsMin.take(15))
print(schemaTempReadingsMax.take(15))

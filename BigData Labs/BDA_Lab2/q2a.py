#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/temperature-readings.csv'
oFile1 = 'outputs/out2_a1'
oFile2 = 'outputs/out2_a2'

#2A
sc = SparkContext(appName = "Lab2_Q2_SparkSQLJob")
sqlContext = SQLContext(sc)

temperature_file = sc.textFile(iFile)
lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.filter(lambda x:(int(x[1][0:4]) >= 1950 and int(x[1][0:4]) <=2014))
temp = temp.map(lambda x: Row(station=x[0], year=int(x[1][0:4]), month = int(x[1][5:7]), temp = float(x[3])))

schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

#API method
schemaNumReadings = schemaTempReadings.filter(schemaTempReadings['temp']>10).groupBy('year','month').count()
schemaNumReadings.rdd.repartition(1).saveAsTextFile(oFile1)
print(schemaNumReadings.take(10))

#Sql method
SQL_NumReadings = sqlContext.sql(
    "SELECT year, month, count(temp) as count FROM tempReadings WHERE temp>10 GROUP BY year, month ORDER BY count DESC")
SQL_NumReadings.rdd.repartition(1).saveAsTextFile(oFile2)
print(SQL_NumReadings.take(10))

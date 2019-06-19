#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/stations-Ostergotland.csv'
iFile2 = 'data/temperature-readings.csv'
oFile = 'outputs/out6'

sc = SparkContext(appName="Lab2_Q6_SparkSQLJob")
sqlContext = SQLContext(sc)

stations = sc.textFile(iFile)
stations = stations.map(lambda line:line.split(";"))
stations = stations.map(lambda x:Row(station=x[0], name=x[1]))
stations = sqlContext.createDataFrame(stations)
stations.registerTempTable("O_Stations")

temperature_file = sc.textFile(iFile2)
lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.filter(lambda x:(int(x[1][0:4]) >= 1950 and int(x[1][0:4]) <=2014))
temp = temp.map(lambda x: Row(station=x[0], year=int(x[1][0:4]), month = int(x[1][5:7]),temp = float(x[3])))
schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

avgMonthTemp = schemaTempReadings.groupBy('year','month','station').agg(F.avg('temp').alias('stationavg'))

#Average monthly temperature in Ostergotland stations
avgMonthTemp = stations.join(avgMonthTemp, "station")
avgMonthTemp = avgMonthTemp.groupBy('year','month').agg(F.avg('stationavg').alias('avgMonthTemp'))

#filtering to find longterm average
longMonthTemp = avgMonthTemp.filter(avgMonthTemp.year <= 1980)
longMonthTemp = longMonthTemp.groupBy("month").agg(F.avg("avgMonthTemp").alias("longAvg"))

#Joining the long term average and monthly average dataframes
MonthlyAvgDiff = avgMonthTemp.join(longMonthTemp, "month")
MonthlyAvgDiff = MonthlyAvgDiff.withColumn("difference",MonthlyAvgDiff.avgMonthTemp-MonthlyAvgDiff.longAvg)
MonthlyAvgDiff = MonthlyAvgDiff.select("year","month","difference").orderBy(["year","month"],ascending=[0,0])

MonthlyAvgDiff = MonthlyAvgDiff.map(lambda line: '%s,%s,%s'%(int(line[0]), int(line[1]), float(line[2])))
print(MonthlyAvgDiff.take(10))

MonthlyAvgDiff.repartition(1).saveAsTextFile(oFile)
print(MonthlyAvgDiff.take(10))

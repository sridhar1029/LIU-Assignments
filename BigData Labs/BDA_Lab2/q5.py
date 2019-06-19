#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/stations-Ostergotland.csv'
iFile2 = 'data/precipitation-readings.csv'
oFile = 'outputs/out5'

sc = SparkContext(appName="Lab2_Q5_SparkSQLJob")
sqlContext = SQLContext(sc)

stations = sc.textFile(iFile)
stations = stations.map(lambda line:line.split(";"))
stations = stations.map(lambda x:Row(station=x[0], name=x[1]))
stations = sqlContext.createDataFrame(stations)
stations.registerTempTable("O_Stations")

preci_file = sc.textFile(iFile2)
plines = preci_file.map(lambda line: line.split(";"))
prec = plines.filter(lambda x:(int(x[1][0:4]) >= 1993 and int(x[1][0:4]) <=2016))
prec = plines.map(lambda x: Row(station=x[0], year = x[1][0:4], month = x[1][5:7], prec = float(x[3])))
schemaPrecReadings =sqlContext.createDataFrame(prec)
schemaPrecReadings.registerTempTable("precReadings")

avgPrec = stations.join(schemaPrecReadings, "station")
avgPrec = avgPrec.groupBy("year", "month", "station").agg(F.sum("prec").alias("monthlyPrec"))
avgPrec = avgPrec.groupBy("year","month").agg(F.avg("monthlyPrec").alias("avgMonthlyPrec")).orderBy(["year", "month"], ascending=[0,0])

avgPrec.rdd.repartition(1).saveAsTextFile(oFile)
print(avgPrec.take(10))

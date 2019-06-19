#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/temperature-readings.csv'
iFile2 = 'data/precipitation-readings.csv'
oFile = 'outputs/out4'

sc = SparkContext(appName = "Lab2_Q4_SparkSQLJob")
sqlContext = SQLContext(sc)

temperature_file = sc.textFile(iFile)
lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.map(lambda x: Row(station=x[0], temp = float(x[3])))
schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

preci_file = sc.textFile(iFile2)
plines = preci_file.map(lambda line: line.split(";"))
prec = plines.map(lambda x: Row(station=x[0], date = x[1], prec = float(x[3])))
schemaPrecReadings =sqlContext.createDataFrame(prec)
schemaPrecReadings.registerTempTable("precReadings")

#finding max temperature and filtering
maxTemp = schemaTempReadings.groupBy('station').agg(F.max('temp').alias('maxtemp'))
maxTemp = maxTemp.filter(maxTemp.maxtemp>25).filter(maxTemp.maxtemp<30)

#finding max daily prec and filtering
dailyPrec = schemaPrecReadings.groupBy('station','date').agg(F.sum('prec').alias('dailyPrec'))
maxDailyPrec = dailyPrec.groupBy('station').agg(F.max('dailyPrec').alias('maxdailyPrec'))
maxDailyPrec = maxDailyPrec.filter(maxDailyPrec.maxdailyPrec>=100).filter(maxDailyPrec.maxdailyPrec<=200)

#joining
StationMax = maxTemp.join(maxDailyPrec, "station").orderBy(['station'],ascending = False)
StationMax.rdd.repartition(1).saveAsTextFile(oFile)
print(StationMax.take(15))

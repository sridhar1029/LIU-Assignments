#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

iFile = 'data/temperature-readings.csv'
oFile3 = 'outputs/out2_b1'
oFile4 = 'outputs/out2_b2'

sc = SparkContext(appName = "Lab2_Q2_SparkSQLJob")
sqlContext = SQLContext(sc)

temperature_file = sc.textFile(iFile)
lines = temperature_file.map(lambda line: line.split(";"))
temp = lines.filter(lambda x:(int(x[1][0:4]) >= 1950 and int(x[1][0:4]) <=2014))
temp = temp.map(lambda x: Row(station=x[0], year=int(x[1][0:4]), month = int(x[1][5:7]), temp = float(x[3])))

schemaTempReadings =sqlContext.createDataFrame(temp)
schemaTempReadings.registerTempTable("tempReadings")

#API method
schemaNumReadings_st = schemaTempReadings.filter(schemaTempReadings['temp']>10).groupBy('year','month').agg(F.countDistinct("station")).orderBy(['count(station)'],ascending = 0)
schemaNumReadings_st.rdd.repartition(1).saveAsTextFile(oFile3)
print(schemaNumReadings_st.take(10))

#SQL Method
SQL_NumReadings_st = sqlContext.sql(
    "SELECT year, month, count(DISTINCT station) as count FROM tempReadings WHERE temp>10 GROUP BY year, month ORDER BY count DESC")
SQL_NumReadings_st.rdd.repartition(1).saveAsTextFile(oFile4)
print(SQL_NumReadings_st.take(10))

#!/usr/bin/env python2
# -*- coding: utf-8 -*-

#Sridhar Adhikarla

from pyspark import SparkContext

iFile = 'data/temperature-readings.csv'
oFile = 'outputs/out3'
fromYear = 1960
toYear = 2014

sc = SparkContext(appName="Lab1_Q3_SparkJob")

lines = sc.textFile(iFile)
lines = lines.map(lambda a: a.split(";"))

observations = lines.filter(lambda observation:
                                       (int(observation[1][:4]) >= fromYear and
                                        int(observation[1][:4]) <= toYear))

stationDailyTemps = observations.map(lambda observation:
                                                ((observation[1], observation[0]),
                                                 (float(observation[3]), float(observation[3]))))

stationDailyMinMaxTemps = stationDailyTemps.reduceByKey(lambda
                                                              (mintemp1, maxtemp1),
                                                              (mintemp2, maxtemp2):
                                                              (min(mintemp1, mintemp2),
                                                               max(maxtemp1, maxtemp2)))

stationMonthlyAvgTemps = stationDailyMinMaxTemps.map(lambda ((day, station), (mintemp, maxtemp)):
                                                           ((day[:7], station), (sum((mintemp, maxtemp)), 2))) \
                                                      .reduceByKey(lambda (temp1, count1), (temp2, count2):
                                                                   (temp1 + temp2, count1 + count2)) \
                                                      .map(lambda ((month, station), (temp, count)):
                                                           ((month, station), temp / float(count)))

stationMonthlyAvgTemps.repartition(1).saveAsTextFile(oFile)

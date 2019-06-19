#!/usr/bin/env python2
# -*- coding: utf-8 -*-

#Sridhar Adhikarla

from pyspark import SparkContext

iFile = 'data/stations-Ostergotland.csv'
iFile2 = 'data/precipitation-readings.csv'
oFile = 'outputs/out5'

sc = SparkContext(appName="Lab1_Q5_SparkJob")

ostergotlandStations = sc.textFile(iFile)
ostergotlandStations = ostergotlandStations.map(lambda line: line.split(";")).map(lambda x: int(x[0])).distinct().collect()

isOstergotlandStation = (lambda s: s in ostergotlandStations)

precipitations = sc.textFile(iFile2)

daily_precipitations = precipitations.map(lambda line: line.split(";")).filter(lambda x: isOstergotlandStation(int(x[0])))
print(daily_precipitations.take(5))
daily_precipitations = daily_precipitations.map(lambda x: (x[0]+','+x[1], float(x[3]))).reduceByKey(lambda a, b: a + b)
print(daily_precipitations.take(5))

st_monthly_precipitations = daily_precipitations.map(lambda x:(x[0].split("-")[0]+','+x[0].split("-")[1], (x[1], 1)))
print(st_monthly_precipitations.take(5))

st_monthly_precipitations = st_monthly_precipitations.reduceByKey(lambda v1, v2: (v1[0] + v2[0], v1[1] + v2[1]))
print(st_monthly_precipitations.take(5))

st_monthly_precipitations = st_monthly_precipitations.map(lambda x: (x[0].split(",")[1] + "," + x[0].split(",")[2], (x[1][0], 1)))# / x[1][1]
print(st_monthly_precipitations.take(5))

monthly_precipitations = st_monthly_precipitations.reduceByKey(lambda v1, v2: (v1[0] + v2[0], v1[1] + v2[1]))
monthly_precipitations = monthly_precipitations.map(lambda x: (x[0], x[1][0] / x[1][1]))
print(monthly_precipitations.take(5))

monthly_precipitations_csv = monthly_precipitations.map(lambda a: '%s,%s' % (a[0], a[1]))
print(monthly_precipitations_csv.take(5))

monthly_precipitations_csv.coalesce(1).saveAsTextFile(oFile)

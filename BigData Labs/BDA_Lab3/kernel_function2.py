#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from pyspark import SparkContext
from datetime import datetime
import collections
from math import radians, cos, sin, asin, sqrt, exp, fabs
import csv

def gaussian(dist, h):
    if isinstance(dist, collections.Iterable):
        res = []
        for x in dist:
            res.append(exp(float(-(x**2))/float((2*(h**2)))))
    else:
        res = exp(float(-(dist**2))/float((2*(h**2))))
    return res

def haversine(lon1, lat1, lon2, lat2):
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a))
    km = 6367 * c
    return km

def timeCorr(time):
    result = []
    if hasattr(time, '__iter__'):
        for x in time:
            if x <= -12:
                result.append(24 + x)
            else:
                result.append(fabs(x))
    else:
        if time <= -12:
            result = 24 + time
        else:
            result = fabs(time)
    return result

def h_hours(time1, time2):
    hDelta = datetime.strptime(time1, '%H:%M:%S') - datetime.strptime(time2, '%H:%M:%S')
    tDiff = hDelta.total_seconds()/3600
    tCorr = timeCorr(tDiff)
    return tCorr

def h_days(day1, day2):
    dDelta = datetime.strptime(day1, '%Y-%m-%d') - datetime.strptime(day2, '%Y-%m-%d')
    return dDelta.days

def mergeVal(x):
    sVals = list(stations_bc.value[x[0]])
    vals = list(x[1])
    vals.extend(sVals)
    return (x[0],tuple(vals))


def kernelFunc(pred, data, dist):
    result = list()

    date = pred["date"]
    lat = pred["lat"]
    lon = pred["lon"]
    times = ['04:00:00', '06:00:00', '08:00:00', '10:00:00', '12:00:00', '14:00:00',
             '16:00:00', '18:00:00', '20:00:00', '22:00:00', '00:00:00']

    data = data.filter(lambda x: datetime.strptime(x[1][0], '%Y-%m-%d') < datetime.strptime(date, '%Y-%m-%d'))

    for time in times:
        temp = data.map(lambda x: (x[1][2],( h_hours(time, x[1][1]),
                                             h_days(date, x[1][0]),
                                             haversine(lon1=lon, lat1=lat, lon2=x[1][4], lat2=x[1][3])))) \
            .map(lambda (temp, (distTime, distDays, distKM)): (temp,(gaussian(distTime, h=dist[0]),
                                                                     gaussian(distDays, h=dist[1]),
                                                                     gaussian(distKM, h=dist[2])))) \
            .map(lambda (temp, (ker1, ker2, ker3)): (temp, ker1 + ker2 + ker3, float(ker1) * float(ker2) * float(ker3))) \
            .map(lambda (temp, kerSum, kerProd): (temp, (kerSum, temp*kerSum, kerProd, temp*kerProd))) \
            .map(lambda (temp, (kerSum, tkSum, kerProd, tkProd)): (None, (kerSum, tkSum, kerProd, tkProd))) \
            .reduceByKey(lambda (kerSum1, tkSum1, kerProd1, tkProd1), (kerSum2, tkSum2, kerProd2, tkProd2): \
                                            (kerSum1+kerSum2, tkSum1+tkSum2, kerProd1+kerProd2, tkProd1+tkProd2)) \
            .map(lambda (key,(sumKer, sumTk, prodKer, prodTk)): (float(sumTk)/float(sumKer), float(prodKer)/float(prodTk)))
        result.append([time, temp.collect()[0]])
    return result



sc = SparkContext(appName = "BDA3_Spark_Kernel_Job")

# Station, lat, long
stations = sc.textFile("data/stations.csv").map(lambda line: line.split(";")) \
                .map(lambda obs: (obs[0], (float(obs[3]), float(obs[4])))).collect()

stations_dict = {}
for s in stations:
    stations_dict[s[0]] = s[1]

#Broadcast stations_dict
stations_bc = sc.broadcast(stations_dict)

# (station, (date, time, temp))
temperatures = sc.textFile("data/temperature-readings.csv") \
                    .sample(False, .001, 12345).map(lambda line: line.split(";")) \
                    .map(lambda l: (l[0], (str(l[1]), str(l[2]), float(l[3]))))


# Test the kernelFunc 
# (station, (date, time, temp, lat, long))
train = temperatures.map(lambda l: mergeVal(l))

pred = {}
pred["date"] = '2013-01-25'
pred["lat"] = 58.4274
pred["lon"] = 14.826

results = {'04:00:00': [[], []], '06:00:00': [[], []], '08:00:00': [[], []], '10:00:00': [[], []],
           '12:00:00': [[], []], '14:00:00': [[], []], '16:00:00': [[], []], '18:00:00': [[], []],
           '20:00:00': [[], []], '22:00:00': [[], []], '00:00:00': [[], []]}

dists = [(2, 5, 50), (2, 5, 70), (2, 7, 100), (3, 8, 100), (4, 9, 150), (4, 10, 200)]
for dist in dists:
    predictions = kernelFunc(pred, train, dist)
    for p in predictions:
        results[p[0]][0].append(p[1][0])
        results[p[0]][1].append(p[1][1])

print(results)

times = ['04:00:00', '06:00:00', '08:00:00', '10:00:00', '12:00:00', '14:00:00',
             '16:00:00', '18:00:00', '20:00:00', '22:00:00', '00:00:00']

with open('outputs/add.csv', 'wb') as out:
    csv_out=csv.writer(out)
    csv_out.writerow(['time', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'])
    for time in times:
        row = [time] + results[time][0]
        csv_out.writerow(row)

with open('outputs/mul.csv', 'wb') as out:
    csv_out=csv.writer(out)
    csv_out.writerow(['time', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'])
    for time in times:
        row = [time] + results[time][1]
        csv_out.writerow(row)

# predictions_rdd = sc.parallelize(predictions).repartition(1)
# print(predictions_rdd.collect())


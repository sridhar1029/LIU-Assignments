#!/usr/bin/env python2
# -*- coding: utf-8 -*-

#Sridhar Adhikarla

import time

iFile = '../data/temperatures-big.csv'
oFile = 'outputs/out1_b_big.csv'
fromYear = 1950
toYear = 2014

start = time.time()
print('Running Python MinMaxTempExtractor:\nFrom %s To %s\nInput file: %s\nOutput file: %s' 
      % (fromYear, toYear, iFile, oFile))
temp_dict = dict()

first_line = False
with open(iFile) as f:
    for l in f:
        if first_line == False:
            print(l)
            first_line = True
            
        line = l.split(";")
        year = int(line[1].split("-")[0])
        if year >= fromYear and year <= toYear:
            temp = temp_dict.get(year)
            station = line[0];
            curr_temp = float(line[3]);
            if not temp:
                #1st list for min temp and 2nd list for max temp
                temp_dict[year] = [[station, curr_temp], [station, curr_temp]]
            else:
                min_temp = float(temp[0][1])
                max_temp = float(temp[1][1])
                if curr_temp < min_temp:
                    temp[0][0] = station
                    temp[0][1] = curr_temp
                if curr_temp > max_temp:
                    temp[1][0] = station
                    temp[1][1] = curr_temp

#sort temperatures descending by max temp 
sorted_temp = temp_dict.items()               
sorted_temp.sort(key=lambda x: x[1][1][1], reverse=True)  

#write the output to file.
with open(oFile,'wb+') as f:
    f.write('%s,%s,%s,%s,%s\n' % ("Year", "Station_Min", "Min_Temp", "Station_Max", "Max_Temp"))
    for i in sorted_temp:
        f.write('%s,%s,%s,%s,%s\n' % (i[0], i[1][0][0], i[1][0][1], i[1][1][0], i[1][1][1]))

end = time.time()
print('Done in %s seconds' % (end - start))
      

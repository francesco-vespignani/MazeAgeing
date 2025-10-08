#!/usr/bin/python3
import json
import sys

itsum = 0

if (len(sys.argv) <= 1):
    print("Usage: rmArr <filename> (without .json extension) ?itemsum")
    exit(-1)
else:
    fn = sys.argv[1]
    if (len(sys.argv) > 2):
       itsum = int(sys.argv[2])
       
with open(fn+'.json', 'r') as file:
    data = json.load(file)

dataout = list()
for i in data: 
    for r in range(0,3):
        i['reps'][r] = i['reps'][r][0]
    i['item'] = itsum + i['item'] 
    dataout.append(i)

with open(fn+"_unnest.json", "w") as write_file:
    json.dump(dataout, write_file, indent=4)


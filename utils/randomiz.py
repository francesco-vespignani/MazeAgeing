#!/usr/bin/python3
import json
import sys
import random

if (len(sys.argv) <= 1):
    print("Usage: rmArr <filename> (without .json extension)")
    exit(-1)
else:
    fn = sys.argv[1]

with open(fn+'.json', 'r') as file:
    data = json.load(file)

random.shuffle(data)

with open(fn+"1.json", "w") as write_file:
    json.dump(data, write_file, indent=4)

random.shuffle(data)

with open(fn+"2.json", "w") as write_file:
    json.dump(data, write_file, indent=4)


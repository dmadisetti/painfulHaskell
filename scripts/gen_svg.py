#!/usr/bin/env python3
import sys
import re
import json
from math import log
import requests

pattern = r"^.*_(\d+)_.*(\d+.\d+)s"

tests = []
times = []

for line in sys.stdin:
    match = re.match(pattern, line)
    test, time = match.groups()
    tests.append(int(test))
    times.append(log(max(float(time), 0.1)))

fastest = min(times)
times = list(map(lambda t: (t - fastest) + 1, times))
slowest = max(times) / 4.0
times = list(map(lambda t: 5 - max(1, int(t / slowest)), times))

results = dict(zip(tests, times))

data = {"data": [], "themes": ["#AAA", "#966fd6"], "option":{"height": 400}}

# create 10x10 grids with gap bricks
for c in range(8):
    ci = (c // 4) * 11
    cj = (c % 4) * 11
    for i in range(0, 10):
        for j in range(0, 10):
            index = c * 100 + i * 10 + j + 1
            if index in results:
                data["data"].append([cj + j, ci + i, results[index], 1])
            else:
                data["data"].append([cj + j, ci + i, 1])

r = requests.post(
    "https://endpointservice.web.app/notebooks/@dmadisetti/contribution-tracker/deployments/contribution",
    json=data)
print(r.content.decode('UTF-8'))

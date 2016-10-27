import random
with open('stations.txt') as station_file:
    stations=station_file.readlines();
    for selection in sorted([random.choice(stations).strip() for i in range(25)]):
        print (selection)
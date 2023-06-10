# -*- coding: utf-8 -*-
"""
Created on Tue Mar 23 16:50:02 2021

@author: yoages
"""

import simpy as sy
import random

class Airport(object):
    
    def __init__(self, env, num_servers, num_scanners):
        
        self.env = env
        self.server = sy.Resource(env, num_servers)
        self.scanners = []
        
        for i in range(0,num_scanners):
            resource = sy.Resource(env, capacity = 1)
            self.scanners.append(resource)

    def checkID(self, passenger):
        ID_service_time = random.expovariate(.75)
        yield self.env.timeout(ID_service_time)

    def scan(self, passenger):
        scan_time = random.uniform(0.5, 1)
        yield self.env.timeout(scan_time)

def Passenger(env, number, s):

    global total_time #global average wait time
    global total_passengers    

    Arrivaltime = env.now

    with s.server.request() as request:
        yield request
        yield env.process(s.checkID(number))
    
    min_q = 0
    for i in range(0, num_scanners):
        if (len(s.scanners[i].queue) < len(s.scanners[min_q].queue)):
            min_q = i
    
    with s.scanners[min_q].request() as request:
        yield request
        yield env.process(s.scan(number))
    
    pass_time = env.now - Arrivaltime
    total_time = total_time + pass_time
    total_passengers = total_passengers+1
 
def setup(env, num, security,arr_rate):
    arrival_int = random.expovariate(arr_rate)
    yield env.timeout(arrival_int)
    env.process(Passenger(env, num, security))

# Setup and start the simulation
random.seed(1)

import pandas as pd
# initialize list of lists
data = [
        [5,5,60,500,5,0,0,0], 
        [10,10,60,500,5,0,0,0], 
        [15,15,60,500,5,0,0,0], 
        [20,20,60,500,5,0,0,0], 
        [25,25,60,500,5,0,0,0], 
        [30,30,60,500,5,0,0,0], 
        [50,50,60,500,5,0,0,0], 
        [20,20,120,500,5,0,0,0], 
        [25,25,120,500,5,0,0,0], 
        [20,20,240,500,5,0,0,0],
        [5,10,30,500,5,0,0,0], 
        [20,40,60,500,5,0,0,0], 
        [10,20,60,500,5,0,0,0], 
        [20,40,120,500,5,0,0,0], 
        [25,50,120,500,5,0,0,0], 
        [20,40,240,500,5,0,0,0],
        [10,5,30,500,5,0,0,0], 
        [40,20,60,500,5,0,0,0], 
        [20,10,60,500,5,0,0,0], 
        [40,20,120,500,5,0,0,0], 
        [50,25,120,500,5,0,0,0], 
        [40,20,240,500,5,0,0,0],
        [2,5,30,500,5,0,0,0], 
        [10,20,60,500,5,0,0,0], 
        [5,10,60,500,5,0,0,0], 
        [10,20,120,500,5,0,0,0], 
        [12,25,120,500,5,0,0,0], 
        [10,20,240,500,5,0,0,0],        
        [5,5,60,500,50,0,0,0], 
        [10,10,60,500,50,0,0,0], 
        [15,15,60,500,50,0,0,0], 
        [20,20,60,500,50,0,0,0], 
        [25,25,60,500,50,0,0,0], 
        [30,30,60,500,50,0,0,0],        
        [50,50,60,500,50,0,0,0], 
        [20,20,120,500,50,0,0,0], 
        [25,25,120,500,50,0,0,0], 
        [20,20,240,500,50,0,0,0],
        [5,10,30,500,50,0,0,0], 
        [20,40,60,500,50,0,0,0], 
        [10,20,60,500,50,0,0,0], 
        [20,40,120,500,50,0,0,0], 
        [25,50,120,500,50,0,0,0], 
        [20,40,240,500,50,0,0,0],
        [10,5,30,500,50,0,0,0], 
        [40,20,60,500,50,0,0,0], 
        [20,10,60,500,50,0,0,0], 
        [40,20,120,500,50,0,0,0], 
        [50,25,120,500,50,0,0,0], 
        [40,20,240,500,50,0,0,0],
        [2,5,30,500,50,0,0,0], 
        [10,20,60,500,50,0,0,0], 
        [5,10,60,500,50,0,0,0], 
        [10,20,120,500,50,0,0,0], 
        [12,25,120,500,50,0,0,0], 
        [10,20,240,500,50,0,0,0]        
        ]

# Create the pandas DataFrame
df = pd.DataFrame(data, columns = ['num_servers', 'num_scanners','runTime','max_passengers','arrival_rate','total_passengers','total_time','avg_time'])

for j in range(0,df.shape[0]):
    print('%d of %d' % (j,df.shape[0]))
    num_servers = df.iloc[j,0]
    num_scanners = df.iloc[j,1]
    runTime = df.iloc[j,2]
    max_passengers = df.iloc[j,3]
    arr_rate = df.iloc[j,4]
        
    total_time = 0
    total_passengers = 0
        
    for i in range(0,50):
        env = sy.Environment()
        ap = Airport(env, num_servers, num_scanners)
        for i in range(0,max_passengers):
            env.process(setup(env, i, ap,arr_rate))
        env.run(until = runTime)
        #print('Average system time: %.1f, Total Time : %d, Total Passengers : %d' % (avg_time, total_time, total_passengers))
    
    df.iloc[j,5] = total_passengers
    df.iloc[j,6] = total_time
    df.iloc[j,7] = total_time/total_passengers

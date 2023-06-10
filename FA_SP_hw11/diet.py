# -*- coding: utf-8 -*-
"""
Created on Thu Apr  1 17:17:14 2021

@author: yoages
"""
#Taken help from -> https://towardsdatascience.com/linear-programming-and-discrete-optimization-with-python-using-pulp-449f3c5f6e99

from pulp import *
import pandas as pd

data = pd.read_excel("D:\OMS\Intro to Analytical Modelling\FA_SP_hw11\data 15.2\diet.xls")
data = data[0:64].values.tolist()

foods = [x[0] for x in data]
cost = dict([(x[0], float(x[1])) for x in data])
calories = dict([(x[0], float(x[3])) for x in data])
chol = dict([(x[0], float(x[4])) for x in data])
fat = dict([(x[0], float(x[5])) for x in data])
sodium = dict([(x[0], float(x[6])) for x in data])
carbs = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protein = dict([(x[0], float(x[9])) for x in data])
vitA = dict([(x[0], float(x[10])) for x in data])
vitC = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])

diet = LpProblem("Diet Problem",LpMinimize)

#set the initial variables
foodVars = LpVariable.dicts("Foods", foods, 0,40,  cat='Integer')

#Add the objective function to mimimize the total cost
diet += lpSum([cost[f]*foodVars[f] for f in foods]), "Total Cost"

#Add in the constraints
diet += lpSum([calories[f]*foodVars[f] for f in foods]) >= 1500, 'min Calories Req.'
diet += lpSum([calories[f]*foodVars[f] for f in foods]) <= 2500, 'max Calories Req.'

diet += lpSum([chol[f]*foodVars[f] for f in foods]) >= 30, 'min Cholesterol Req.'
diet += lpSum([chol[f]*foodVars[f] for f in foods]) <= 240, 'max Cholesterol Req.'

diet += lpSum([fat[f]*foodVars[f] for f in foods]) >= 20, 'min fat Req.'
diet += lpSum([fat[f]*foodVars[f] for f in foods]) <= 70, 'max fat Req.'

diet += lpSum([sodium[f]*foodVars[f] for f in foods]) >= 800, 'min sodium Req.'
diet += lpSum([sodium[f]*foodVars[f] for f in foods]) <= 2000, 'max sodium Req.'

diet += lpSum([carbs[f]*foodVars[f] for f in foods]) >= 130, 'min Carbs Req.'
diet += lpSum([carbs[f]*foodVars[f] for f in foods]) <= 450, 'max Carbs Req.'

diet += lpSum([fiber[f]*foodVars[f] for f in foods]) >= 125, 'min fiber Req.'
diet += lpSum([fiber[f]*foodVars[f] for f in foods]) <= 250, 'max fiber Req.'

diet += lpSum([protein[f]*foodVars[f] for f in foods]) >= 60, 'min protein Req.'
diet += lpSum([protein[f]*foodVars[f] for f in foods]) <= 100, 'max protein Req.'

diet += lpSum([vitA[f]*foodVars[f] for f in foods]) >= 1000, 'min vitA Req.'
diet += lpSum([vitA[f]*foodVars[f] for f in foods]) <= 10000, 'max vitA Req.'

diet += lpSum([vitC[f]*foodVars[f] for f in foods]) >= 400, 'min vitC Req.'
diet += lpSum([vitC[f]*foodVars[f] for f in foods]) <= 5000, 'max vitC Req.'

diet += lpSum([calcium[f]*foodVars[f] for f in foods]) >= 700, 'min calcium Req.'
diet += lpSum([calcium[f]*foodVars[f] for f in foods]) <= 1500, 'max calcium Req.'

diet += lpSum([iron[f]*foodVars[f] for f in foods]) >= 10, 'min iron Req.'
diet += lpSum([iron[f]*foodVars[f] for f in foods]) <= 40, 'max iron Req.'

#Solve and print results
print("Solving Diet Problem...")
diet.solve()
print("Status:", LpStatus[diet.status])

for v in diet.variables():
    if (v.varValue != 0.0 and v.name.startswith('Foods_')): #Only print items that are not zero
        print (v.name, "=", v.varValue)

print ("Total Cost is $%.2f" % value(diet.objective))

############################################################################## Part 2 ###################################################################

chosenFood = LpVariable.dicts("Chosen",foods,0,1,"Integer")

for f in foods:
    diet += foodVars[f] <= 1000 * chosenFood[f]
    diet += foodVars[f] >= .1 * chosenFood[f]
    
# add contraints to eat at most one of a group of foods    
diet += chosenFood['Frozen Broccoli'] + chosenFood['Celery, Raw'] <=1.0, 'At most 1 Brocolli and Celery'

# add further contraints
diet += chosenFood['Tofu'] + chosenFood['Roasted Chicken'] + chosenFood['Poached Eggs'] + chosenFood['Scrambled Eggs'] + chosenFood['Bologna,Turkey']\
        + chosenFood['Frankfurter, Beef'] + chosenFood['Ham,Sliced,Extralean'] + chosenFood['Kielbasa,Prk'] + chosenFood['Hamburger W/Toppings']\
        + chosenFood['Hotdog, Plain'] + chosenFood['Pork'] + chosenFood['White Tuna in Water']  >= 3.0, 'At least 3 proteins'


#Solve and print results
print("Solving Diet Problem...")
diet.solve()
print("Status:", LpStatus[diet.status])

for v in diet.variables():
    if (v.varValue != 0.0 and v.name.startswith('Foods_')): #Only print items that are not zero
        print (v.name, "=", v.varValue)

print ("Total Cost is $%.2f" % value(diet.objective))


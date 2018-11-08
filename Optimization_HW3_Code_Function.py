from numpy import *
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import math
import random

def optimizer1D(func, x1, s):
    TAU = (5 ** 0.5 - 1) / 2
    EP_X = 1e-4
    EP_F = 1e-6
    POINT_NUM = 1000
    EP_M = 2.220446049250313e-16
    f1 = func(x1)
    count = 0
    x2 = x1 + s
    f2 = func(x2)
    count += 1
    if f2 > f1:
        x1, x2 = x2, x1
        f1, f2 = f2, f1
        s = -s

    while True:
        s = s / TAU
        x4 = x2 + s
        f4 = func(x4)
        count += 1
        if f4 > f2:
            break
        x1, x2 = x2, x4
        f1, f2 = f2, f4
    f_bar = 0

    while True:
        x3 = TAU * x4 + (1 - TAU) * x1
        f3 = func(x3)
        count += 1
        if (f2 < f3):
            x4, x1 = x1, x3
        else:
            x1, x2, f2 = x2, x3, f3
        f_old = f_bar
        f_bar = (f1 + f2 + f3) / 3
        if abs(x1 - x4) <= EP_M ** 0.5 * abs(x2) + EP_X or abs(f_bar - f_old) <= EP_M ** 0.5 * abs(f2) + EP_F:
            break
    return x2, func(x2),count

def Disturbance(s,x_old,allowed,jud):
    if jud == 1:#dis
        x_new = x_old + np.random.uniform(-1,1)*s
        #allowed = [0,1]
        return dis(x_new,allowed)
    else:
        if jud == 0:#con and with range
            x_new = x_old + np.random.uniform(-1,1)*s
            #allowed = [0,1]
            if x_new > max(allowed) or x_new < min(allowed):
                return np.random.uniform(min(allowed),max(allowed))
            else:
                return x_new
        else:
            return x_old

def dis(x,allowed):
    min = abs(allowed[0]-x)
    temp = x
    for i in range(len(allowed)):
        if abs(allowed[i] - x) <= min:
            temp = allowed[i]
            min = abs(allowed[i] - x)
    return temp
        
    
def g(a):
    c = 2
    if a > 0.6:
        return 1 + c*(a-0.6)/0.4
    else:
        if a < 0.4:
            return 1/(1+c*(0.4-a)/0.4)
        else:
            return 1
        
def gm(a,S):
    S=g(a)*S
    return S        
        
def simulated_annealing(func, x0, allowed, t0, t_ﬁnal,jud):
#simulated annealing routine.
#inputs:
# func - the objective function 
# x0 - the initial state 
# allowed: a list, where allowed[i] is the list of allowed values for x[i].
# allowed[i] is the empty list if x[i] is a real-valued variable, rather than 
# a discrete variable.
# t0: the initial temperature 
# t_ﬁnal: the ﬁnal temperature 
#--------------------------------
#STUDENT CODE GOES BELOW:
    curr_temp = t0 
    curr_state = x0 
    curr_score = func(curr_state) 
    best = list(x0)
    best_score = curr_score 
    new_state = x0
    Nt=5
    Nc=200
    rt=0.9
    rs=0.9
    St=1.0
    S=np.ones(len(x0))
    a=np.ones(len(x0))
    while curr_temp > t_final:
        counter_t = 1
        #print(1)
        while counter_t <= Nt:
            counter_c = 1
            #print(2)
            while counter_c <= Nc:
                #print(curr_state,best,best_score)
                for i in range(len(x0)):
#                     if len(allowed[i]) > 0:
                        new_state[i] = Disturbance(S[i],curr_state[i],allowed[i],jud[i])
                new_score = func(new_state)
                #print(3)
                if new_score <= curr_score:
                    curr_state = new_state
                    curr_score = new_score
                    #print(4)
                    if new_score <= best_score:
                        best = list(curr_state)
                        best_score = new_score
                        #print(best,best_score)
                else:
                    #print(best,best_score)
                    p =  math.exp((curr_score-new_score)/curr_temp)
                    r = np.random.uniform(0,1)
                    if r < p:
                        curr_state = new_state
                        curr_score = new_score
                        #print(6)
                    else:
                        for i in range(len(a)):
                            a[i] = a[i] - 1/Nc
                counter_c = counter_c + 1
                #print(best,best_score)
            counter_t = counter_t + 1
            for i in range(len(S)):
                S[i] = gm(a[i],S[i])
        curr_temp = curr_temp*rt
        for i in range(len(S)):
            S[i] = S[i]*rs
#STUDENT CODE ENDS 
#--------------------------------
    #print(best)
    return best, best_score, counter_c

def sta():
    return np.round(np.random.uniform(0,1))
#def simulated_annealing(func, x0, allowed, t0, t_ﬁnal,jud):

# def dis(x,y):#plus function
#     n = len(x)
#     c = x
#     for i in range(n):
#         c[i] = x[i] - y[i]
#     return c

def nor(a):# norm of a vector
    n = len(a)
    s = 0
    for i in range(n):
        s = s + a[i]**2
    return np.sqrt(s)

def eval_dis(func,allowed,jud,true_val):
    x_value = []
    f_value = []
    c_value = []
    i = 0
    starting_temp = 10.0 
    ending_temp = 0.1 
    POINT_NUM = 50
    while i < POINT_NUM:
        starting = [sta(),sta(),sta(),sta(),sta(),sta(),1.5]
        x_val,f_val,c_val = simulated_annealing(func=func,x0=starting,allowed=allowed,t0=starting_temp,t_final=ending_temp,jud=jud)
        #print(c_val)
        c_value.append(c_val)
        #print(f_val)
        f_value.append(f_val)
        #print(x_val)
        err = []
        for j in range(len(true_val)):
            d = true_val[j] - x_val[j]
            err.append(d)
        x_value.append(nor(err))
        i +=1
        #print(i)
    return np.mean(x_value), np.var(x_value)**0.5, np.mean(f_value), np.var(f_value)**0.5, np.mean(c_value), np.var(c_value)**0.5
    #return np.mean(f_value), np.var(f_value)**0.5, np.mean(c_value), np.var(c_value)**0.5

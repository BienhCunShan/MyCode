import math
import random
import time


def MEp():
    f = 0.5
    temp = 0.0
    while 1 + f > 1:
        temp = f
        f /= 2
    EP_M = temp
    print("\nMachine precision is", EP_M)


MEp()

TAU = (5 ** 0.5 - 1) / 2
EP_X = 1e-4
EP_F = 1e-6
POINT_NUM = 1000
EP_M = 2.220446049250313e-16


# optimizer
def optimizer1D(func, x1, s):
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
    return x2, count


def optimizer2D(func, x1, y1, s):
    count = 0

    def funcx(x):
        return func(x, y1)

    def funcy(y):
        return func(x1, y)

    while (True):
        x2, n2 = optimizer1D(funcx, 0, s)
        y2, m2 = optimizer1D(funcy, 0, s)
        count = count + n2 + m2
        y = func(x1, y1)
        y0 = func(x2, y2)
        count += 2
        x1, y1 = x2, y2
        if (abs(y - y0) <= EP_M ** 0.5 * abs(y0) + EP_F):
            break
    return x2, y2, count


def eval1(func, ture_value):
    number = []
    tatime = []
    value = []
    count = 0
    while count < POINT_NUM:
        start = random.uniform(-1, 1)
        start_time = time.time()
        val, num = optimizer1D(func, start, 0.3)
        end_time = time.time()
        number.append(num)
        value.append(abs((val - ture_value) / ture_value))
        tatime.append((end_time - start_time) * 1000)
        count += 1

    # calculate average value
    sum = [0, 0, 0]
    aver = [0, 0, 0]
    for i in range(POINT_NUM):
        sum[0] += number[i]
        sum[1] += tatime[i]
        sum[2] += value[i]
    aver[0] = sum[0] / POINT_NUM
    aver[1] = sum[1] / POINT_NUM
    aver[2] = sum[2] / POINT_NUM

    sum = [0, 0, 0]
    s = [0, 0, 0]
    for i in range(POINT_NUM):
        sum[0] += (number[i] - aver[0]) ** 2
        sum[1] += (tatime[i] - aver[1]) ** 2
        sum[2] += (value[i] - aver[2]) ** 2
    s[0] = (sum[0] / (POINT_NUM - 1)) ** 0.5
    s[1] = (sum[1] / (POINT_NUM - 1)) ** 0.5
    s[2] = (sum[2] / (POINT_NUM - 1)) ** 0.5
    # print "s::", s
    print("[number of evaluations]:",)
    print("x=", aver[0], "\ty=", s[0])
    print("[running time]:",)
    print("x=", aver[1], "\ty=", s[1])
    print("[relative distance]:",)
    print("x=", aver[2], "\ty=", s[2])


def eval2(func, ture_value1, ture_value2):
    number = []
    tatime = []
    value1 = []
    value2 = []
    count = 0
    while count < POINT_NUM:
        x1 = random.uniform(-1, 1)
        y1 = random.uniform(-1, 1)
        start_time = time.time()
        val1, val2, num = optimizer2D(func, x1, y1, 0.3)
        end_time = time.time()
        number.append(num)
        value1.append(abs((val1 - ture_value1) / ture_value1))
        value2.append(abs((val2 - ture_value2) / ture_value2))
        tatime.append((end_time - start_time) * 1000)
        count += 1

    # calculate average value
    sum = [0, 0, 0, 0]
    aver = [0, 0, 0, 0]
    for i in range(POINT_NUM):
        sum[0] += number[i]
        sum[1] += tatime[i]
        sum[2] += value1[i]
        sum[3] += value2[i]
    aver[0] = sum[0] / POINT_NUM
    aver[1] = sum[1] / POINT_NUM
    aver[2] = sum[2] / POINT_NUM
    aver[3] = sum[3] / POINT_NUM

    sum = [0, 0, 0, 0]
    s = [0, 0, 0]
    for i in range(POINT_NUM):
        sum[0] += (number[i] - aver[0]) ** 2
        sum[1] += (tatime[i] - aver[1]) ** 2
        sum[2] += (value1[i] - aver[2]) ** 2 + (value2[i] - aver[3]) ** 2
    s[0] = (sum[0] / (POINT_NUM - 1)) ** 0.5
    s[1] = (sum[1] / (POINT_NUM - 1)) ** 0.5
    s[2] = (sum[2] / (POINT_NUM - 1)) ** 0.5
    # print "s::", s
    print("[number of evaluations]:",)
    print("x=", aver[0], "\ty=", s[0])
    print("[running time]:",)
    print("x=", aver[1], "\ty=", s[1])
    print("relative distance:",)
    print("x=", aver[2], "\ty=", s[2])


# problem1
def func1(x):
    return x ** 2 - 2 * x + 1


def func2(x):
    return math.exp(x + 2) - x


print("\nProblem1",)
#optimizer1D(func1, random.uniform(-1, 1), 0.3)
#optimizer1D(func2, random.uniform(-1, 1), 0.3)
print("\nfunction1",)
eval1(func1, 1)
print("\nfunction2",)
eval1(func2, -2)


# problem2
def func1(x, y):
    return (x - 1) ** 2 + (y - 1) ** 2


def func2(x, y):
    return (x - 1) ** 2 + math.exp(y + 2) - y


print("\nProblem2",)
#optimizer2D(func1, 1, 1, 0.3)
#optimizer2D(func2, 1, 1, 0.3)
print("\nfunction1",)
eval2(func1, 1, 1)
print("\nfunction2",)
eval2(func2, 1, -2)

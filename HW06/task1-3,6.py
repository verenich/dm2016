# Copyright Lisa Yankovskaya
import random
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


N_tables = 1000
sum_numbers = 1000


'''task2'''
def random_values(sum_table, n = 4):
    points = sorted(random.sample(xrange(1, sum_table), n - 1))
    return [a - b for a, b in zip(points + [sum_table], [0] + points)]

def create_rules_2(N, M):
    random.seed(59)
    df = pd.DataFrame(index = range(0, N), columns=['rule'])
    for i in range(0, N):
        df['rule'][i] = random_values(M)
    return df

df = create_rules_2(N_tables, sum_numbers)

def calculate_scloj(dataframe):
    rules = dataframe['rule']
    dataframe.loc[:,'support'] = pd.Series('NA', index = dataframe.index)
    dataframe.loc[:,'confidence'] = pd.Series('NA', index = dataframe.index)
    dataframe.loc[:,'lift'] = pd.Series('NA', index = dataframe.index)
    dataframe.loc[:,'odds ratio'] = pd.Series('NA', index = dataframe.index)
    dataframe.loc[:,'jaccard'] = pd.Series('NA', index = dataframe.index)
    index = 0
    for rule in rules:
	sum_item = sum(rule)
        sup = float(rule[0]) / sum_item
        dataframe.set_value(index, 'support', sup)
        conf = float(rule[0]) / (rule[0] + rule[1])
        dataframe.set_value(index, 'confidence', conf)
        lift = float(rule[0] * sum_item) / ((rule[0] + rule[2]) * (rule[0] + rule[1]))
        dataframe.set_value(index, 'lift', lift)
        if rule[1] != 0 and rule[2] != 0:
            odd = float((rule[0]*rule[3]))/(rule[1] * rule[2])
            dataframe.set_value(index, 'odds ratio', odd)
        else:
            dataframe.set_value(index, 'odds ratio', 'NA')
        jacc = float(rule[0]) / (rule[0] + rule[1] + rule[2])
        dataframe.set_value(index, 'jaccard', jacc)
        index += 1
    return dataframe

df = calculate_scloj(df)

print "top rules by support"
print df.sort_index(by = ['support'], ascending=[False])[0:10]
print "top rules by confidence"
print df.sort_index(by = ['confidence'], ascending=[False])[0:10]
print "lift"
print df.sort_index(by = ['lift'], ascending=[False])[0:10]
print "top rules by odds ratio"
print df.sort_index(by = ['odds ratio'], ascending=[False])[0:10]
print "top rules by jaccard"
print df.sort_index(by = ['jaccard'], ascending=[False])[0:10]

'''task 3'''
def create_rules_variable(vector, position, N = 1001):
    df = pd.DataFrame(index = range(0, N), columns=['rule'])
    for i in range(0, N):
        vector[position] = i
        df['rule'][i] = list(vector)
    return df

N_x = np.arange(0, 1001, 1)
df1 = create_rules_variable([0, 200, 200, 200], 0)
df2 = create_rules_variable([200, 0, 200, 200], 1)
df3 = create_rules_variable([200, 200, 0, 200], 2)
df4 = create_rules_variable([200, 200, 200, 0], 3)

df1 = calculate_scloj(df1)
df2 = calculate_scloj(df2)
df3 = calculate_scloj(df3)
df4 = calculate_scloj(df4)

plt.plot(N_x, df1['lift'], label = 'f11', linewidth = 2)
plt.plot(N_x, df2['lift'], label = 'f10', linewidth = 5)
plt.plot(N_x, df3['lift'], label = 'f01', linewidth = 2)
plt.plot(N_x, df4['lift'], label = 'f00', linewidth = 2)
plt.ylabel('Lift')
plt.legend()
# plt.savefig('3_lift.png')
plt.show()


'''task6'''
def create_rules_6(N, M):
    random.seed(59)
    df = pd.DataFrame(index = range(0, N), columns=['rule', 'f11', 'f10', 'f01', 'f00'])
    for i in range(0, N):
        rule = random_values(M)
        df['rule'][i] = rule
        df['f11'][i] = rule[0]
        df['f10'][i] = rule[1]
        df['f01'][i] = rule[2]
        df['f00'][i] = rule[3]
    return df

df = create_rules_6(N_tables, sum_numbers)
df = calculate_scloj(df)
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
cmap = matplotlib.cm.get_cmap("hot")
l = ax.scatter(df.f11, df.f10, df.f01, c = df.jaccard, cmap = cmap)
fig.colorbar(l)
ax.set_xlabel('f11')
ax.set_ylabel('f10')
ax.set_zlabel('f01')
ax.set_xlim(0, 1000)
ax.set_ylim(0, 1000)
ax.set_zlim(0, 1000)
plt.show()

plt.figure(facecolor = '#F5F5F5')
cmap = matplotlib.cm.get_cmap("hot") # chage color type of color scale
plt.subplot(111)
plt.scatter(df.support, df.lift, s = 30, c = df.confidence, cmap = cmap)
plt.colorbar()
plt.xlabel("support")
plt.ylabel("lift")
plt.xlim(0,1)
plt.ylim(0, 0.03)
plt.show()

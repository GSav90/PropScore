import random
import numpy as np
import pandas as p
import csv
import os

def main():
    data_dir = os.path.join('C:\\Users\Gaurav\PycharmProjects', 'MDT')
    Xtrain = np.genfromtxt(os.path.join(data_dir, 'test1.csv'), delimiter=',',skip_header=1)

    #Xtrain = p.read_csv('test1.csv',encoding='utf-8')
    print(len(Xtrain))
    l1=[x for x in range(1,51)]
    l2=[]
    for i in range(len(l1)):
        l1[i]=l1[i]/10
    prob = ([0.2/30] * 30) + ([0.05] * 10) + ([0.03] * 10)
    print(len(l1))
    print(len(prob))
    for j in range(len(Xtrain)):
        l2.append(np.random.choice(l1,p=prob))
    print(Xtrain.shape)
    l2=np.array(l2)
    l2=l2.reshape(len(l2),1)
    print('l2: ',l2)
    #Xtrain_new=np.concatenate((Xtrain,l2),axis=0)
    Xtrain_new = np.hstack((Xtrain,l2))
    print(Xtrain_new)
    # filePath = 'C:\Users\Gaurav\PycharmProjects\MachineLearningHomework\Hw9\\'
    np.savetxt('With_SR.csv',Xtrain_new)
    #np.savetxt(filePath+'v.csv',v)
main()
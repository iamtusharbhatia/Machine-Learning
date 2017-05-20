from __future__ import division
import json, random, math, sys

args = sys.argv

data = []
with open(args[3]) as f:
    for line in f:
        data.append(json.loads(line))
        
arr_data = [[0 for x in range(2)] for x in range(251)]
i = 0
for val in data:
    arr_data[i][0] = val['id']
    arr_data[i][1] = val['text']
    i = i + 1

k = int(args[1])   
centroids = []
f1 = open(args[2], 'r+')
for val in f1:
    centroids.append(val)
f1.close()

j = 0
for val in centroids:
    centroids[j] = centroids[j][0:18]
    j = j + 1
centroids = list(centroids)
centroids = random.sample(centroids, k)

#Removing Punctuations
i = 0 
for val in arr_data:
    arr_data[i][1] = "".join(c for c in arr_data[i][1] if c not in ('!','.',':','@','#'))
    i = i + 1  

def initialFunc(arr_data, centroids):
    for items in range(1,26):
        j = 0
        for val in centroids:
            centroids[j] = centroids[j][0:18]
            j = j + 1
        curr_cluster = []
        i = 0
        for d in centroids:
            curr_cluster.append([])    
            curr_cluster[i].append(d)
            i = i + 1
        conv = minDist(arr_data, centroids, curr_cluster)
        if(conv == 'converged'):
            f = open(args[4], 'w+')
            i = 1001
            for item in curr_cluster:
                item = [int(n) for n in item]
                f.write("%s" % i)
                f.write(' --> ')
                f.write("%s\n\n" % item[1:])
                i = i + 1
            f.close()
            sum = calcSse(arr_data,curr_cluster)
            print('SSE Value', sum)
            break
        
def calcSse(arr_data,curr_cluster):
    sum = 0
    for v1 in curr_cluster:
        val1 = v1[0]
        for v2 in  v1:
            d = calcDist(arr_data, val1, v2)
            sum = sum + d
    return sum



def calcDist(arr_data, t_id1, t_id2):
    tweet1 = ''
    tweet2 = ''
    
    for val in arr_data:
        if(int(val[0]) == int(t_id1)):
            tweet1 = val[1]
    
    for val in arr_data:
        if(int(val[0]) == int(t_id2)):
            tweet2 = val[1]
            
    set1 = set(tweet1.split(' '))
    set2 = set(tweet2.split(' '))
    
    setIntersect = list(set(set1) & set(set2))
    setUnion = list(set(set1) | set(set2))
    
    num = len(setUnion) - len(setIntersect)
    den = len(setUnion)
    dist= num/den
    return dist   

def minDist(arr_data, centroids, curr_cluster):
    for val in arr_data:
        minDistance = 1
        for c_p in curr_cluster:
            d = calcDist(arr_data, val[0], c_p[0])
            if(d <= minDistance):
                minDistance = d
                id = c_p[0]
        
        i = 0
        for v in curr_cluster:
            if(v[0] == id):
                curr_cluster[i].append(val[0])
            i = i + 1
    conv = reCalcCent(arr_data, centroids, curr_cluster)
    return conv

def reCalcCent(arr_data, centroids, curr_cluster):
    temp_cent = []
    k = 0
    for c in centroids:
        temp_cent.append(c)
        k = k + 1
    i = 0
    for val in curr_cluster:
        temp_cluster = val[1:]
        minDist = 10000
        for v1 in temp_cluster:
            sum = 0
            for v2 in  temp_cluster:
                d = calcDist(arr_data, v1, v2)
                sum = sum + d
            if (sum <= minDist):
                minDist = sum
                id = v1
        centroids[i] = str(id)
        i = i + 1
    if(temp_cent == centroids):
        conv = 'converged'
        return conv
    return

initialFunc(arr_data, centroids)                  
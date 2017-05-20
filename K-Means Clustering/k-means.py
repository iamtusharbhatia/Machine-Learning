import csv, random, math, sys

args = sys.argv

data_file = open(args[2])
dataSet   = csv.reader(data_file)
data      = list(dataSet)
del data[0]

c = int(args[1])  
cluster_list = []
mean_values  = []
old_mean     = []

def calcSse(cluster_list,mean_values):
    v = 0
    sum = 0
    for c_id in cluster_list:
       n = 0
       for i in c_id:
           for val in data:
               if(i == val[0]):
                   n = n + 1
                   dist = math.pow((float(val[1]) - float(mean_values[v][1])),2) + math.pow((float(val[2]) - float(mean_values[v][2])),2)
                   sum = sum + dist
       v = v + 1                 
    return sum

def reCalcCent(data,cluster_list,mean_values):
    v = 0
    for c_id in cluster_list:
       n = 0
       sum_x = 0
       sum_y = 0
       for i in c_id:
           for val in data:
               if(i == val[0]):
                   n = n + 1
                   sum_x = sum_x + float(val[1])
                   sum_y = sum_y + float(val[2])
       mean_values[v][1] = sum_x / n
       mean_values[v][2] = sum_y / n
       v = v + 1                         
    return mean_values       
            


def minDist(data,cluster_list,mean_values):
    
    for val in data:
        minDistance = 100
        for c_p in mean_values:
            dist = math.pow((math.pow((float(val[1]) - float(c_p[1])),2) + math.pow((float(val[2]) - float(c_p[2])),2)),0.5)
            if(dist <= minDistance):
                minDistance = dist
                c_id = c_p[0]
        count = 0
        for xyz in cluster_list:
            if(xyz[0] == c_id):
                cluster_list[count].append(val[0])
            else:
                count = count + 1
    return cluster_list
        

def kmeans(data, c):
    L = random.sample(data, c)
    i = 0
    j = 1001
    for d in L:
        cluster_list.append([])
        cluster_list[i].append(j)
        
        mean_values.append([])
        mean_values[i].append(j)
        mean_values[i].append(L[i][1])
        mean_values[i].append(L[i][2])       
        i = i+1
        j = j +1
    count1 = 0
    for f in range(1,26):
        minDist(data,cluster_list,mean_values)
        u = 0
        for q in mean_values:
            old_mean.append([])
            old_mean[u].append(q[0])
            old_mean[u].append(q[1])
            old_mean[u].append(q[2])
            u = u + 1
                    
        reCalcCent(data,cluster_list,mean_values)
        
        if(old_mean == mean_values):
            Sum = calcSse(cluster_list,mean_values)
            
            f = open(args[3], 'w+')
            for item in cluster_list:
                item = [int(n) for n in item]
                f.write("%s" % item[0])
                f.write(' --> ')
                f.write("%s\n\n" % item[1:])
            f.close()
            print('SSE Value',Sum)
            break
        
        count1 = count1 + 1
        if(count1 >= 25):
            Sum = calcSse(cluster_list,mean_values)
            
            f = open(args[3], 'w+')
            for item in cluster_list:
                item = [int(n) for n in item]
                f.write("%s" % item[0])
                f.write(' --> ')
                f.write("%s\n\n" % item[1:])
            f.close()          
            print('SSE Value',Sum)
        else:
            a = 0
            p = 1001
            del cluster_list [0:]
            for d in L:
                cluster_list.append([])
                cluster_list[a].append(p)
                a = a + 1
                p = p + 1
        del old_mean [0:]
    
kmeans(data, c)
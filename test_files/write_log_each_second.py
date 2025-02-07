import sys
import os
import time

cnt=0

for i in range(100):
    f = open(r'log.log', 'a')
    for j in range(10):
        cnt+=1
        time.sleep(1)
        f.write(str(cnt)+'\n')
        f.flush()
        print('write', cnt)
    f.close()
    time.sleep(3)


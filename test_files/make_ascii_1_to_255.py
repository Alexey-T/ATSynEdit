#!python3
f=open('ascii_1_to_255.txt', 'wb')
for i in range(256):
    if i>0:
        f.write(i.to_bytes(1, 'little'))
       
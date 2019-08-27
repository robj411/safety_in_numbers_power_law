import os 
import csv
import sys

motorists = int(sys.argv[1])
print motorists,'motorists'
cyclists = int(sys.argv[2])
print cyclists,'cyclists'
framesize = int(sys.argv[3])
print 'frame size =', framesize
duration = int(sys.argv[4])
print 'duration =', duration
showwindow = sys.argv[5]=='T'
print 'show window =', showwindow

import simulate_city
import junction

fields = ['motorists','cyclists','sidelength','collisions']
with open(r'outputs.csv','w') as f:
    writer = csv.writer(f)
    writer.writerow(fields)


STEP_SIZE = 25
WINDOW_WIDTH = int(float(framesize)/5.0 * STEP_SIZE)
WINDOW_HEIGHT = int(float(framesize)/5.0 * STEP_SIZE)


x = junction.main_function(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,duration,cyclists,motorists,showwindow)
print(x)

STEP_SIZE = 5
WINDOW_WIDTH = framesize * STEP_SIZE
WINDOW_HEIGHT = framesize * STEP_SIZE


x = simulate_city.main_function(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,1,cyclists,motorists,showwindow)
print(x)

with open('outputs.csv','a') as f:
    writer = csv.writer(f)
    writer.writerow([motorists,cyclists,framesize,x])

#with open(r'inputs.csv','r') as f:
#    openfile = csv.reader(f, delimiter=',', quotechar='|')
#    i = 1
#    next(openfile, None) 
#    for line in openfile:
        #call = ' '.join(['python PacMan.py',line[1],line[2],line[3],line[4]])
        #os.system(call)
#        motorists = int(line[1])
#        cyclists = int(line[2])
#        framesize = int(line[3])
#        showwindow = line[4]=='T'
#        WINDOW_WIDTH = framesize * STEP_SIZE
#        WINDOW_HEIGHT = framesize * STEP_SIZE
#        x = simulate_city.main_function(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,1,cyclists,motorists,showwindow)
#        print(x)
#        with open('outputs.csv','a') as f:
#            writer = csv.writer(f)
#            writer.writerow([motorists,cyclists,framesize,x])
#        i = i+1
#        if i==2:
#            sys.exit()

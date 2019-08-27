

import random
import time
import numpy

import pygame
from pygame.locals import *

class Common(object):
    def __init__(self,x,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        self.x = x
        self.lastmove = 0
        self.WINDOW_WIDTH = WINDOW_WIDTH
        self.WINDOW_HEIGHT = WINDOW_HEIGHT
        self.STEP_SIZE = STEP_SIZE
        self.start_position = numpy.random.choice(range(4), size=1)
        self.turn = numpy.random.choice(range(3), size=1)
        if(self.start_position==0):
            self.x = (WINDOW_WIDTH-STEP_SIZE)/2
            self.y = WINDOW_HEIGHT
        if(self.start_position==1):
            self.x = 0
            self.y = (WINDOW_HEIGHT-1*STEP_SIZE)/2
        if(self.start_position==2):
            self.x = (WINDOW_WIDTH+3*STEP_SIZE)/2
            self.y = 0
        if(self.start_position==3):
            self.x = WINDOW_WIDTH
            self.y = (WINDOW_HEIGHT+3*STEP_SIZE)/2
        self.direction = (self.start_position - 2) % 4
        self.second_direction = (self.start_position+self.turn+1) % 4
        self.x1 = (self.WINDOW_WIDTH-2*self.STEP_SIZE)/2
        self.x2 = self.x1 + 4*self.STEP_SIZE
        self.y1 = (self.WINDOW_HEIGHT-2*self.STEP_SIZE)/2
        self.y2 = self.y1 + 4*self.STEP_SIZE

    # Function draws the icon
    def draw(self, surface):
        pygame.draw.rect(surface, self.colour, [self.x, self.y, self.STEP_SIZE, self.STEP_SIZE], 0)

    # Function for movement
    def update(self):
        turn = (self.start_position==0 & self.y < self.y1)
        #if(self.turnCount >= (self.WINDOW_WIDTH/2)/self.STEP_SIZE & turn):
        #    self.direction = self.second_direction
        self.updateCount = self.updateCount + 1
        if self.updateCount > self.updateCountMax:

            # Updates the position of the ghost
            if self.direction == 0: # Down
                if self.y + self.STEP_SIZE >= self.WINDOW_HEIGHT:
                    self.y = self.WINDOW_HEIGHT - self.STEP_SIZE
                else:
                    self.y = self.y + self.STEP_SIZE
            if self.direction == 1: # Left
                if self.x - self.STEP_SIZE < 0:
                    self.x = 0
                else:
                    self.x = self.x - self.STEP_SIZE
            if self.direction == 2: # Up
                if self.y - self.STEP_SIZE < 0:
                    self.y = 0
                else:
                    self.y = self.y - self.STEP_SIZE

            if self.direction == 3: # Right
                if self.x + self.STEP_SIZE >= self.WINDOW_WIDTH:
                    self.x = self.WINDOW_WIDTH - self.STEP_SIZE
                else:
                    self.x = self.x + self.STEP_SIZE

            self.updateCount = 0
            self.turnCount = self.turnCount + 1

# Defines the class for the bike (pacman)
class Bike(Common):
    x = 0
    y = 0
    direction = 0

    updateCountMax = 2
    updateCount = 0
    turnCount = 0
    colour = (255,0,255)

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        Common.__init__(self,"Bike",WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE)

# Defines the class for the ghost
class Motor(Common):
    x = 0
    y = 0
    direction = 0

    updateCountMax = 2
    updateCount = 0
    turnCount = 0
    colour = (255,255,0)

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        Common.__init__(self,"Motor",WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE)


# Defines the class for the Game
class Game:

    # Function determines whether two objects have collided by comparing where the origin corners of
    # the shape are and the total size of each object 
    def isCollision(self, x1, y1, x2, y2, bsize):
        if x1 < x2 + bsize and x1 + bsize > x2 and y1 < y2 + bsize and y1 + bsize > y2:
                return True
        return False

# Defines the class app the controls all the logic and game function
class App(object):
    count = 0

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,DURATION,cyclists,motorists,showwindow):
        self._running = True
        self._display_surf = None
        #self._image_surf = None
        #self._pacman_surf = None
        self.windowWidth = WINDOW_WIDTH
        self.windowHeight = WINDOW_HEIGHT
        self.STEP_SIZE = STEP_SIZE
        self.DURATION = DURATION
        self.cyclists = cyclists
        self.motorists = motorists
        self.showwindow = showwindow
        #if showwindow:
        #    import pygame
        #    from pygame.locals import *

        self.game = Game()
        self.bike = []
        #for i in range(cyclists):
        #    self.bike.append(Bike(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE))
        self.motor = []
        #for i in range(motorists):
        #    self.motor.append(Motor(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE))

    # Function initializes all imported pygame modules, sets screen size and caption, and loads image resources
    def on_init(self):
        pygame.init()
        self._display_surf = pygame.display.set_mode((self.windowWidth, self.windowHeight), pygame.HWSURFACE)

        pygame.display.set_caption('Collision simulation')
        self._running = True
        #picture = pygame.image.load("ghost.png")
        #picture = pygame.transform.scale(picture, (self.STEP_SIZE, self.STEP_SIZE))
        #self._image_surf = picture#.convert()
        #picture = pygame.image.load("pacman.png")
        #picture = pygame.transform.scale(picture, (self.STEP_SIZE, self.STEP_SIZE))
        #self._pacman_surf = picture.convert()
        #rect = picture.get_rect()

    # Function determines the game logic for how things move
    def on_loop(self):
        #for i in range(len(self.bike)):
        #    self.bike[i].random()
        #for i in range(len(self.motor)):
        #    self.motor[i].random()
        for i in range(len(self.bike)):
            self.bike[i].update()
        for i in range(len(self.motor)):
            self.motor[i].update()
        for i in reversed(range(len(self.bike))):
            if(self.bike[i].turnCount>=self.windowWidth/self.STEP_SIZE):
                del self.bike[i]
        for i in reversed(range(len(self.motor))):
            if(self.motor[i].turnCount>=self.windowWidth/self.STEP_SIZE):
                del self.motor[i]

        # Determines collision happened.
        for j in range(len(self.motor)):
            for i in reversed(range(len(self.bike))):
                if self.game.isCollision(self.bike[i].x, self.bike[i].y, self.motor[j].x, self.motor[j].y, self.STEP_SIZE):
                    self.count += 1     # Increase count by one
                    del self.bike[i]#self.bike[i] = Bike(self.windowWidth,self.windowHeight,self.STEP_SIZE)
        pass

    # Function draws the game components to screen
    def on_render(self):
        self._display_surf.fill((0, 0, 0)) 
        x1 = (self.windowWidth-2*self.STEP_SIZE)/2
        x2 = x1 + 4*self.STEP_SIZE
        y1 = (self.windowHeight-2*self.STEP_SIZE)/2
        y2 = y1 + 4*self.STEP_SIZE
        pygame.draw.line(self._display_surf, (255, 255, 255), [x1,0], [x1, y1],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [x2,0], [x2, y1],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [x1,self.windowHeight], [x1, y2],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [x2,self.windowHeight], [x2, y2],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [0,y1], [x1, y1],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [0,y2], [x1, y2],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [self.windowWidth,y1], [x2, y1],10)
        pygame.draw.line(self._display_surf, (255, 255, 255), [self.windowWidth,y2], [x2, y2],10)
        for i in range(len(self.bike)):
            self.bike[i].draw(self._display_surf)     # Draws bikes
        for i in range(len(self.motor)):
            self.motor[i].draw(self._display_surf)    # Draws motorists
        myfont = pygame.font.SysFont('arial', 20)
        displaytext = "Count: " + str(self.count)
        textsurface = myfont.render(displaytext, False, (255, 255, 255))
        self._display_surf.blit(textsurface,(0,0))
        pygame.display.flip()                                       # Updates the display surface to screen

    # Function uninitializes all pygame modules
    def on_cleanup(self):
        pygame.quit()

    def on_execute(self):
        if self.showwindow == True:
            if self.on_init() == False:
                self._running = False
        
        iterations = 0;
        cycle_start_times = numpy.random.choice(range(self.DURATION), size=self.cyclists)
        car_start_times = numpy.random.choice(range(self.DURATION), size=self.motorists)
        while (self._running and iterations < 500):
            
            for i in cycle_start_times:
                if(i==iterations):
                    self.bike.append(Bike(self.windowWidth,self.windowHeight,self.STEP_SIZE))
            for i in car_start_times:
                if(i==iterations):
                    self.motor.append(Motor(self.windowWidth,self.windowHeight,self.STEP_SIZE))
            iterations += 1
            if self.showwindow == True:
                pygame.event.pump()

            self.on_loop()
            if self.showwindow == True:
                self.on_render()

            #time.sleep(10.0 / 1000.0);
        if self.showwindow == True:
            self.on_cleanup()


def main_function(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,DURATION,cyclists,motorists,showwindow):
    theApp = App(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,DURATION,cyclists,motorists,showwindow)
    theApp.on_execute()
    return(theApp.count)




